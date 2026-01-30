#Library load message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "mnsentinellakes: Please note that as of January 2026, multiple changes have been made to this package to update terminology.
    Primarily references to lake ids and lake names have been updated to basin ids and basin names. Additionally the function to
    download water quality data from the mnpca has been temporarily disabled."
  )
}
