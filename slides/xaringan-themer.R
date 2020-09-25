library(xaringanthemer)

style_solarized_dark(
  text_color = "#F6F6F6",                      # gray
  header_color = "#99B2DD",                    # light-blue
  background_color = "#002b36",                # default bg color
  link_color = "#99B2DD",                      # light-blue
  text_bold_color = "#FF9300",                 # orange
  code_highlight_color = "#E9D968",            # yellow
  code_inline_color = "#8A8A8A",               # gray
  inverse_background_color = "#FFFFFF",        # white
  inverse_text_color = "#002b36",              # default bg color
  inverse_header_color = "#99B2DD",            # light-blue
  title_slide_text_color = "#99B2DD",          # light-blue
  title_slide_background_color = "#002b36",    # default bg color
  left_column_subtle_color = "#586E75",        # some gray
  left_column_selected_color = "#93A1A1",      # some lighter gray
  blockquote_left_border_color = "#E9D968",    # yellow
  table_border_color = "#8A8A8A",              # gray
  table_row_border_color = "#8A8A8A",          # gray
  table_row_even_background_color = "#353535", # slightly lighter dark-blue
  base_font_size = "26px",
  header_h1_font_size = "1.75rem",
  header_h2_font_size = "1.5rem",
  header_h3_font_size = "1.25rem",
  text_font_google = google_font("Source Sans Pro"),
  text_font_family = xaringanthemer_font_default("text_font_family"),
  text_font_weight = xaringanthemer_font_default("text_font_weight"),
  text_font_url = xaringanthemer_font_default("text_font_url"),
  text_font_family_fallback = xaringanthemer_font_default("text_font_family_fallback"),
  text_font_base = "sans-serif",
  code_font_google = google_font("Source Sans Code"),
  code_font_family = xaringanthemer_font_default("code_font_family"),
  code_font_size = "0.7rem",
  code_font_url = xaringanthemer_font_default("code_font_url"),
  code_font_family_fallback = xaringanthemer_font_default("code_font_family_fallback"),
  outfile = "xaringan-themer.css"
)
