erllambda SAM example
=====================

This is a sample AWS Lambda function built using Erlang for
demonstration purposes: how to build and deploy AWS Lambda with a provided runtime
using [AWS SAM](https://github.com/awslabs/serverless-application-model) and `erllambda` library.

## Packaging and deployment

This project is built using [rebar3_erllambda](https://github.com/alertlogic/rebar3_erllambda) plugin.
Please see [getting started](https://github.com/alertlogic/rebar3_erllambda#getting-started)
section on how to bootstrap development environment.

### Create a package

Create a package as it's shown in [create package](https://github.com/alertlogic/rebar3_erllambda#creating-a-package) section.

If environment is configured correctly it should be as easy as:

``` shell
git clone https://github.com/alertlogic/erllambda_sam_example.git
cd erllambda_sam_example
rebar3 get-deps
rebar3 compile
rebar3 release
rebar3 erllambda zip
```

**Note**: you might want to run `compile`, `release` and `erllambda zip` commands from [erllambda docker](https://github.com/alertlogic/erllambda_docker) container as it's described in [rebar3_erllambda readme](https://github.com/alertlogic/rebar3_erllambda#using-the-erllambda-docker-container) since some artifacts are sensitive to a target platform. 

### Run lambda locally

Before function is deployed in to AWS there's an option to start function locally and verify that it works as expected without creating all necessary resources in AWS.
This possibility provided by AWS `sam` tool.

#### Local package

The only difference in a local package and a package that is created for production deployment is a
config file, that is shipped with erlang release. For local stack package will be packaged with
[local-sys.config](config/local-sys.config), which has a section for `erlcloud` library that allows
to modify AWS config variables to route calls to AWS services to a specified host.

In this example dynamodb will be started on a local machine in a docker container. In order to be
able to talk to dynamodb from lambda container, `erlcloud` library should be configured to point to
a docker host address, which can route all requests to a process listening on a provided port.

``` erlang
{erlcloud,
  [{aws_config,
    [{ddb_scheme, "http://"},
     {ddb_host, "host.docker.internal"},
     {ddb_port, 8000}]}
  ]}
```

#### Build a package for local stack

Build local package as you would do it for AWS stack but specify `local` profile for `rebar3` commands:

``` shell
rebar3 as local compile
rebar3 as local release
rebar3 as local erllambda release
```

#### Start dependencies

This example requires running dynamodb to function properly.
For the local stack we are going to use [official dynamodb](https://hub.docker.com/r/amazon/dynamodb-local/) docker image.

``` shell
docker pull amazon/dynamodb-local
```

To be able to talk to dynamodb instance from multiple hosts, container should be started with a shared DB:

``` shell
docker run --rm -v `pwd`/_data:/var/dynamodb_data -d -p 8000:8000 --name dynamodb \
    amazon/dynamodb-local -jar DynamoDBLocal.jar -sharedDb -dbPath /var/dynamodb_data
```

Create dynamodb table for the lambda function. Table will be created automatically by AWS cloudformation template, but for local stack this is a manual process:

``` shell
aws dynamodb create-table                                    \
    --endpoint http://localhost:8000                         \
    --table-name test-table                                  \
    --attribute-definitions AttributeName=id,AttributeType=S \
    --key-schema AttributeName=id,KeyType=HASH               \
    --provisioned-throughput ReadCapacityUnits=10,WriteCapacityUnits=10
```

#### Start function locally

Using `sam local` start local AWS Lambda runtime with a configured in the template API Gateway:

``` shell
TABLE_NAME=test-table sam local start-api \
    -t ./etc/local-template.yaml --region us-east-1
```

#### Call local API Gateway

Once `sam local` has been initialised and started your function will be accessible on port `3000`:

``` shell
$ curl http://localhost:3000/resource -XPOST -d '{"id": "bar", "field": "bar-field"}'
[]
$ curl http://localhost:3000/resource
[{"field":"bar-field","id":"bar"}]
```

### Deploy

1. We need a `S3 bucket` where we can upload our Lambda
   functions packaged as ZIP before we deploy anything - If you don't
   have a S3 bucket to store code artifacts then this is a good time to
   create one:

    ``` shell
    aws s3 mb s3://BUCKET_NAME
    ```

2. Package lambda function to S3:

    ``` shell
    sam package \
        --template-file etc/template.yaml    \
        --output-template-file packaged.yaml \
        --s3-bucket REPLACE_THIS_WITH_YOUR_S3_BUCKET_NAME
    ```

3. create a Cloudformation Stack and deploy your SAM resources.

    ``` shell
    sam deploy \
        --template-file packaged.yaml      \
        --stack-name erllambda-sam-example \
        --capabilities CAPABILITY_IAM
    ```

### Call deployed function through API Gateway

To get a deployed API Gateway Endpoint URL run the following command:

``` shell
aws cloudformation describe-stacks     \
    --stack-name erllambda-sam-example \
    --query 'Stacks[].Outputs'
```

This should return URL similar to `https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/MyResource/resource`

#### Create an item

``` shell
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource \
    -XPOST -d '{"id": "foo", "field": "foo"}'
[]
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource \
    -XPOST -d '{"id": "bar", "field": "bar"}'
[]
```

#### Get an item by id

``` shell
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource/foo
{"id":"foo","field":"foo"}
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource/bar
{"id":"bar","field":"bar"}
```

#### Get all items

``` shell
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource
[{"id":"foo","field":"foo"},{"id":"bar","field":"bar"}]
```

#### List items matching a given filter

``` shell
$ curl 'https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource?field=bar'
[{"id":"bar","field":"bar"}]
```

#### Update item with a provided id

``` shell
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource/bar \
    -XPUT -d '{"field": "new-bar"}'
[]
```

#### Delete item with a given id

``` shell
$ curl 'https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource/foo \
    -XDELETE
[]
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource
[{"id":"bar","field":"new-bar"}]
```

### Delete stack

When you do not need stack anymore you can delete it using the following command:

``` shell
aws cloudformation delete-stack --stack-name erllambda-sam-example
```

## How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
github issue or simply submit a PR directly that implements your improvement.

For complex changes, or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


## How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a github issue.

<!--- vim: sw=4 et ts=4 -->
