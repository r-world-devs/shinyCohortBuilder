packer::bundle()

output_js <- "./inst/www/scb.js"

if (file.exists(output_js)) file.remove(output_js)
lapply(
  list.files("./srcjs", pattern = "*.js", full.names = TRUE),
  file.append, file1 = output_js
)

library(sass)
output_css <- "./inst/www/scb.css"
css_bundle <- do.call(
  sass_bundle,
  lapply(
    list.files("./srcscss", pattern = "*.scss", full.names = TRUE),
    sass_file
  )
)
sass(
  css_bundle,
  options = sass_options(output_style = "compressed"),
  output = "./inst/www/scb.min.css"
)
sass(
  css_bundle,
  output = output_css
)
rm(css_bundle, output_js, output_css)
