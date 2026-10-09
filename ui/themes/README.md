# Colour schemes

The scheme files from [tinted-theming/schemes](https://github.com/tinted-theming/schemes)
(commit `a70da1dab18008023cfd55a94053f3b6cab4f86e`), unchanged, under its MIT [LICENSE](LICENSE).

`pal-ui` reads every `*/*.yaml` here at compile time (see `ui/Ui/Themes.hs`) and
offers them in the page's theme menu. To update, replace `base16/`, `base24/` and
`tinted8/` with the upstream directories and rebuild.
