code <- qrcode::qr_code("https://github.com/shared-research/multiverse-meta-analysis")
qrcode::generate_svg(code, here::here("conferences", "sips2023", "poster", "img", "qrcode.svg"))
code <- magick::image_read(here::here("conferences", "sips2023", "poster", "img", "qrcode.svg"))
magick::image_write(code, here::here("conferences", "sips2023", "poster", "img", "qrcode_final.svg"))
