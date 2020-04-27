An exploration app for COVID-19 reproduction number estimation using data from https://github.com/CSSEGISandData/2019-nCoV built in Shiny and using [R0](https://cran.r-project.org/web/packages/R0/index.html) for estimation.

----

Definition of [Reproduction Number](https://en.wikipedia.org/wiki/Basic_reproduction_number)

The application uses the [Time Dependent](https://www.frontiersin.org/articles/10.3389/fvets.2017.00046/full) method of R0 estimation.

----

It has been published [here](https://covid-explorer-pa6ye47i4a-ew.a.run.app/).

----

Building A Docker Image
-----------------------

Inside the covid-19-explorer directory you can build the Docker image:

> docker build -t extropy-covid19-explorer .

This may take several minutes the first time.

You can run it as follows:

> docker run -p 8080:8080 extropy-covid19-explorer


Deploying To Google Cloud Run
-----------------------------

To upload the image (once built as above) to Google Cloud Run:

> docker build . --tag gcr.io/PROJECT_ID/IMAGE_NAME

This deploys to the project called PROJECT_ID and gives it the name IMAGE_NAME

This can be deployed when creating a service in Google Cloud Run.

