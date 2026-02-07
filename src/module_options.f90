module module_options
    implicit none

    type :: option_t
        character(len=20) :: flag
        character(len=20) :: arg_type
        character(len=200) :: description
        character(len=10) :: required   ! "required" o "optional"
        character(len=100) :: default
    end type option_t

    type(option_t), parameter :: OPTIONS(*) = [ &
        option_t("--dem",     "PATH",       "DEM raster file. Horizontal and vertical units must be meters",                  "required",   ""),  &
        option_t("--rain",    "FLOAT|PATH", "Uniform rain intensity [mm/h] or raster rain file",                              "optional",  ""), &
        option_t("--esl",     "FLOAT",      "Extreme sea level relative to duration (--d)",                                   "optional",  ""), &
        option_t("--d",       "FLOAT",      "Duration of rain or esl event [s] (default: 3600)",                              "optional",  "3600"), &
        option_t("--ti",      "INT",        "Output interval [s] (e.g., 3600 = 1 snapshot per hour)",                         "optional",  ""), &
        option_t("--tmax",    "INT",        "Simulation stop time [s] (default: 3600)",                                       "optional",  "3600"), &
        option_t("--delt",    "INT",        "Time step delta [s] (default: 600)",                                             "optional",  "600"), &
        option_t("--nl",      "INT",        "Number of pixels per cell side (default: 100)",                                  "optional",  "100"), &
        option_t("--man",     "FLOAT",      "Manning coefficient (default: 0.02)",                                            "optional",  "0.02"), &
        option_t("--force",   "",           "Force preprocessing even if .grd file exists",                                   "optional",  ""), &
        option_t("--tmp",     "PATH",       "Temporary directory",                                                            "optional",  ""), &
        option_t("--out",     "PATH",       "Water depth output raster (default: <dem>.wd@<time>.tif)",                       "optional",  "<dem>.wd@<time>.tif"), &
        option_t("--max",     "PATH",       "Maximum water depth output raster",                                              "optional",  ""), &
        option_t("--seamask", "PATH",       "Sea mask raster (1=sea, 0=land)",                                                "optional",  ""), &
        option_t("--verbose", "",           "Enable verbose output",                                                          "optional",  ""), &
        option_t("--version", "",           "Display version information",                                                    "optional",  ""), &
        option_t("--debug",   "",           "Enable debug mode with detailed output",                                         "optional",  ""), &
        option_t("--help",    "",           "Display this help message",                                                      "optional",  "")  &
    ]

end module module_options
