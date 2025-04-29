-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = "Catppuccin Mocha"
config.enable_tab_bar = false -- disable tab bar

-- ref: https://wezfurlong.org/wezterm/config/font-shaping.html
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }

-- and finally, return the configuration to wezterm
return config
