# Shorten filepaths for mock API requests.
function (request) {
  # NGD mocks
  request <- httptest::gsub_request(request,
                                    "https://api.os.uk/features/ngd/ofa/v1/collections/",
                                    "NGD",
                                    fixed = TRUE)

  request <- httptest::gsub_request(request,
                                    "bld-fts-buildingline-1",
                                    "",
                                    fixed = TRUE)

  request <- httptest::gsub_request(request,
                                    "items//",
                                    "",
                                    fixed = TRUE)

  # Places mocks
  request <- httptest::gsub_request(request,
                                    "https://api.os.uk/search/places/v1/",
                                    "Places/",
                                    fixed = TRUE)
}
