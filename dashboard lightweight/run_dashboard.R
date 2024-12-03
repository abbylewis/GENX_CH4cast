install.packages("rsconnect")
install.packages("quarto")
install.packages("shiny")
install.packages("here")
library(quarto)

#Update files
unlink(here::here("dashboard lightweight","data","*"))
copy_target <- file.copy(here::here("L1_target.csv"), here::here("dashboard lightweight", "data"), overwrite = T)
outputs <- list.files(here::here("outputs"), full.names = T)
outputs <- outputs[grepl(Sys.Date(), outputs)]
copy_outputs <- file.copy(outputs, here::here("dashboard lightweight", "data"), overwrite = T)

# Deploy
quarto_publish_app(input = here::here("dashboard lightweight"), 
                   server = "shinyapps.io")
