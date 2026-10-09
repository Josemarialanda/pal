// Vercel function for POST /api/run.
//
// Starts the bundled pal-ui binary once per function instance (on 127.0.0.1,
// a free port) and forwards each request to it. pal-ui only accepts a Host
// header of 127.0.0.1:PORT, so that is what we send.
const { spawn } = require("child_process");
const http = require("http");
const path = require("path");

const root = path.join(__dirname, "..", "pal");
let server = null; // Promise<port>

function startServer() {
  return new Promise((resolve, reject) => {
    const child = spawn(
      path.join(root, "lib", "ld-linux-x86-64.so.2"),
      ["--library-path", path.join(root, "lib"), path.join(root, "pal-ui"), "--port", "0", "--no-open"],
      { stdio: ["ignore", "pipe", "pipe"], env: { ...process.env, LANG: "C.UTF-8", NO_COLOR: "1", TERM: "dumb" } },
    );
    let out = "";
    child.stdout.on("data", (d) => {
      out += d;
      const m = out.match(/127\.0\.0\.1:(\d+)/);
      if (m) resolve(Number(m[1]));
    });
    child.stderr.on("data", (d) => console.error("pal-ui:", String(d)));
    child.on("error", reject);
    child.on("exit", (code) => {
      server = null;
      reject(new Error("pal-ui exited with code " + code));
    });
  });
}

function forward(port, body) {
  return new Promise((resolve, reject) => {
    const req = http.request(
      { host: "127.0.0.1", port, method: "POST", path: "/api/run", headers: { Host: `127.0.0.1:${port}`, "Content-Type": "text/plain; charset=utf-8", "Content-Length": body.length } },
      (res) => {
        const chunks = [];
        res.on("data", (c) => chunks.push(c));
        res.on("end", () => resolve({ status: res.statusCode, type: res.headers["content-type"], body: Buffer.concat(chunks) }));
      },
    );
    req.on("error", reject);
    req.setTimeout(20000, () => req.destroy(new Error("timeout")));
    req.end(body);
  });
}

function readBody(req, limit) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on("data", (c) => {
      size += c.length;
      if (size > limit) reject(Object.assign(new Error("too large"), { status: 413 }));
      else chunks.push(c);
    });
    req.on("end", () => resolve(Buffer.concat(chunks)));
    req.on("error", reject);
  });
}

module.exports = async (req, res) => {
  if (req.method !== "POST") {
    res.statusCode = 405;
    return res.end("method not allowed");
  }
  try {
    const body = await readBody(req, 1024 * 1024);
    if (!server) server = startServer();
    let port;
    try {
      port = await server;
    } catch (e) {
      server = startServer();
      port = await server;
    }
    const r = await forward(port, body);
    res.statusCode = r.status;
    res.setHeader("Content-Type", r.type || "application/json; charset=utf-8");
    res.setHeader("Cache-Control", "no-store");
    res.end(r.body);
  } catch (e) {
    res.statusCode = e.status || 500;
    res.end(e.status ? "request too large" : "internal error: " + e.message);
  }
};
