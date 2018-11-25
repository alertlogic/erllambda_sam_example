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
git clone git@github.com:alertlogic/erllambda_sam_example.git
cd erllambda_sam_example
rebar3 get-deps
rebar3 compile
rebar3 release
rebar3 erllambda zip
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
$ curl https://ea5728392s9.execute-api.us-east-1.amazonaws.com/Prod/resource/bar
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
