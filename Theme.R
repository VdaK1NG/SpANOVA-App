# Cambiar tema
library("fresh")
create_theme(
  # Aquí cambiamos los colores default
  adminlte_color( 
    light_blue = NULL, # Primary
    yellow = NULL, # Warning
    aqua = NULL, # Info
    red = NULL,
    green = NULL,
    blue = NULL,
    navy = NULL,
    teal = NULL,
    olive = NULL,
    lime = NULL,
    orange = NULL,
    fuchsia = "#FF2F1B",
    purple = "#FF2F1B",
    maroon = "#FF2F1B",
    black = NULL,
    gray_lte = NULL
    
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#133BF2", # Fondo
    dark_hover_bg = NULL,
    dark_color = NULL,
    dark_hover_color = "white",
    dark_submenu_bg = "#7189F7", # Fondo submenu
    dark_submenu_color = "white",
    dark_submenu_hover_color = "white",
    light_bg = "#FF2F1B",
    light_hover_bg = "#FF2F1B",
    light_color = "#FF2F1B",
    light_hover_color = "#FF2F1B",
    light_submenu_bg = "#FF2F1B",
    light_submenu_color = "#FF2F1B",
    light_submenu_hover_color = "#FF2F1B"
  ),
  
  adminlte_global(
    content_bg = "#c4cfff", # Background contenido general
    box_bg = "white",  # Background boxes
    info_box_bg = "#7189F7"
  ),
  output_file = "www/mytheme.css"
)
