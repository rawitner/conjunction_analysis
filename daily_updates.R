setwd("~/Documents/Centauri/fall part time/conjunction_analysis/")

library(gmailr)
gm_auth_configure(path = "~/Documents/Centauri/fall part time/credentials.json")
gm_auth()

mssgs <- gm_messages("Conjunction Reports",num_results = 1) # get most recent email
ids = gm_id(mssgs)
Mn = gm_message(ids[1])
attachments = gm_attachments(Mn)
attachment = filter(attachments, grepl("1day", filename))
fname = as.character(attachment$filename)
attachment_id = as.character(attachment$id)

my_attachment = gm_attachment(id=attachment_id, message_id=Mn$id)
gm_save_attachment(my_attachment, paste0("conj_data/", fname)) # my_attachment must be the actual attachment
#### 
## TEST

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")

rmarkdown::render("index.Rmd", output_dir = "docs")

quit(save="no")