This app can be easily turned into a docker image and uploaded to Google Cloud Run.


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

