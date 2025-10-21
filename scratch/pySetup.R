# This is a setup script for setting the environment for any R scripts that
# call python functions, specifically rgee (R google earth engine) which is
# an R wrapper for a python wrapper to GEE :D.

library(reticulate)

# --- CONFIGURATION ---
ENV_PATH <- file.path(getwd(), "env")
PYTHON_VERSION <- "3.10"
REQUIRED_PACKAGES <- c(
  "earthengine-api",
  "numpy"
)

# ---------------------

# Check if the Conda environment already exists

# NOTE: 'conda_list()' returns a data frame of existing environments.
# We check if the 'env' path is in the 'python' column.
env_exists <- any(conda_list()$python == file.path(ENV_PATH, "bin/python"))


if (env_exists) {
  # if environment exists: activate it
  message("Conda environment found. Activating...")
  use_miniconda(ENV_PATH)
  print("Conda environment activated successfully.")

} else {

  # if environment does NOT exist: create and install packages
  message("Conda environment not found. Creating and installing packages...")

  # Ensure Miniconda is installed before creating the environment
  tryCatch(invisible(conda_version()), # if no conda, this will return an error
           error = function(e) {
             message("Installing Miniconda...")
             install_miniconda(python_version = PYTHON_VERSION)
           })

  # Create the environment
  conda_create(
    envname = ENV_PATH,
    python_version = PYTHON_VERSION
  )

  # Install all required packages
  conda_install(
    envname = ENV_PATH,
    packages = REQUIRED_PACKAGES,
    pip = TRUE # Use pip for packages like 'earthengine-api' if they're not on conda-forge
  )

  # Activate the newly created environment
  use_condaenv(ENV_PATH, required = TRUE)
  print("Conda environment created and activated successfully. If you encounter errors, please restart your R session after initial environment creation.")

}
