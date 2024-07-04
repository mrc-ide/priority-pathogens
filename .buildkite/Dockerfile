FROM rocker/r-ver:4

RUN Rscript -e 'install.packages("pak")'

# For some reason pak fails to install urltools
RUN Rscript -e 'install.packages("urltools")'

COPY .buildkite/pkgdepends.txt /src/.buildkite/pkgdepends.txt
RUN Rscript -e 'pak::pkg_install(readLines("/src/.buildkite/pkgdepends.txt"))'

COPY . /src
