library(connectapi)
client <- connect(server = Sys.getenv("CONNECT_SERVER"),
                  api_key = Sys.getenv("CONNECT_API_KEY"))

file.copy(from = "./docs/articles/logo.png", 
          to = "./docs/reference/logo.png")

rsconnect::writeManifest(appDir = "./docs")
bundle <- bundle_dir("./docs")

content <- client %>% 
  deploy(bundle, name = "HASP_docs") %>% 
  poll_task()