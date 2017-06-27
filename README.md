Fizzbuzz
========

This is an example implementation of a Fizzbuzz on top of a JSON API.

It allows fetching individual numbers and also lists of them using pagination.

It aims to follow the JSON API specification V 1.0 (http://jsonapi.org/)

Dependencies
-
It is expected to have Erlang/OTP installed and available in the $PATH. (version 17.4 +).
The easiest way to install Erlang/OTP is probably using Kerl: http://github.com/kerl/kerl

When building, dependencies will be fetched from Github (see rebar.config file).

How to build and run
-
With:
```
$./rebar3 release
```
will generate a release, that can be started with:
```
$ ./_build/default/rel/fizzbuzz/bin/fizzbuzz
```
The server will be listening in port `8080` on `localhost`

Usage
-
To fetch a single nunmber, you can send a `GET` request to the following path

```
/numbers/NUMBER
```
For example, the following `GET` request:

```
http://localhost:8080/numbers/30
```
will return:
```
{"data":{"type":"numbers","id":"30","attributes":{"value":"Fizz Buzz","favourite":"false"}}}
```
For pagination, an example would be:

```
http://localhost:8080/numbers?page[number]=1&page[size]=4
```
which will return:

```
{"meta":{"total-pages":25000000000},"data":[{"type":"numbers","id":"1","attributes":{"value":"1","favourite":"false"}},{"type":"numbers","id":"2","attributes":{"value":"2","favourite":"false"}},{"type":"numbers","id":"3","attributes":{"value":"Fizz","favourite":"false"}},{"type":"numbers","id":"4","attributes":{"value":"4","favourite":"false"}}]}
```
Updating a resource
-
The application supports updating resources (not creating them) by using PATCH requests. The resources attribute `favourite` is the only one that can be updated. Requests are not expected to include the `value` attribute.

The following `PATCH` request will mark the number 3 as favourite:

```
PATCH /numbers/3 HTTP/1.1
Host: localhost:8080
Content-Type: application/json

{"data":{"type":"numbers","id":"3","attributes":{"favourite":"true"}}}
```
Running Eunit tests
-
For running the Eunit tests:
```
$ ./rebar3 eunit
```

Generating coverage report
-
After running Eunit tests, the following command will generate an HTML coverage report:

```
$ ./rebar3 cover
```
