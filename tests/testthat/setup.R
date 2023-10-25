library(httptest)

# For contexts where the package needs to be fooled
# no need for a real secret
if (!nzchar(Sys.getenv('OS_API_KEY'))) {
  Sys.setenv(OS_API_KEY = "foobar")
}
