source("./R/libraries.R", local = TRUE, encoding = c("UTF-8"))

loadAllLibraries()

# TODO: Revisar esto
# font_import()
# loadfonts(device = "win")

i18n <- Translator$new(translation_json_path='./data/translation.json')
i18n$set_translation_language('en')