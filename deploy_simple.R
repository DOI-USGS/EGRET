library(connectapi)
client <- connect(server = Sys.getenv("CONNECT_SERVER"),
                  api_key = Sys.getenv("CONNECT_API_KEY"))

file.copy(from = "./public/articles/logo.png", 
          to = "./public/reference/logo.png")

rsconnect::writeManifest(appDir = "./public")
bundle <- bundle_dir("./public")

content <- client %>% 
  deploy(bundle, name = "EGRET") %>% 
  poll_task()