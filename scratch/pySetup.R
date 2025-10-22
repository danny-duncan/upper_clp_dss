# This is a setup script for setting the environment for any R scripts that
# call python functions, specifically rgee (R google earth engine) which is
# an R wrapper for a python wrapper to GEE :D.

library(reticulate)

# --- CONFIGURATION ---
ENV_PATH <- file.path(getwd(), "env")
PYTHON_VERSION <- "3.10.13"
REQUIRED_PACKAGES <- c(
  "earthengine-api==1.6.0",
  "pandas==2.0.3",
  "pyreadr==0.5.2",
  "pyyaml==6.0.2",
  "numpy==1.24.4"
)
# ---------------------

# 1. Check if the Conda environment already exists
# NOTE: 'conda_list()' returns a data frame of existing environments.
# We check if the 'env' path is in the 'python' column.
env_exists <- any(conda_list()$python == file.path(ENV_PATH, "bin", "python")) # Adjust for OS differences if needed

if (env_exists) {
  # 2. Environment exists: Just activate it
  message("Conda environment found. Activating...")
  use_condaenv(ENV_PATH, required = TRUE)
  print("Conda environment activated successfully.")

  # OPTIONAL: You can check if all packages are installed here and install them if they're missing,
  # but for simplicity and stability, we'll assume a successful initial setup.

} else {
  # 3. Environment does NOT exist: Create and install packages
  message("Conda environment not found. Creating and installing packages...")

  # Ensure Miniconda is installed before creating the environment
  if (!conda_version()) {
    message("Installing Miniconda...")
    install_miniconda()
  }

  # Create the environment (reticulate handles the path and python version)
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
  print("Conda environment created and activated successfully. You may need to restart R for full effect.")
}

# 4. Final check: if an environment was just created, a restart might be required
# to fully attach reticulate to the new Python session. This message is optional.
if (reticulate::py_available(initialize = TRUE) && !env_exists) {
  message("NOTE: If you encounter errors, please restart your R session after initial environment creation.")
}

