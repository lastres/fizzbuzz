#! /bin/env python
import sys, argparse, httplib, json

def parseargs():
    parser = argparse.ArgumentParser(
        description='Query the Fizbuzz server JSON API.')
    parser.add_argument('-n', '--number', help='A resource number')
    parser.add_argument('-f', '--favourite', help='A number to make favourite')
    parser.add_argument(
        '-u', '--unfavourite', help='A number to make no favourite')
    parser.add_argument(
        '-p', '--page', metavar=('PAGE', 'SIZE'), help='Page number and size',
        nargs=2)
    return parser.parse_args()

def printnumber(data):
    """ Print a singe number JSON data"""
    print "Id:", data["id"], "Favourite:", data["attributes"]["favourite"], \
        "Value:", data["attributes"]["value"]

def printerrorjson(jsondata):
    """ Print the ERROR JSON values returned by the server"""
    print "Error! Status: ", jsondata["errors"]["status"], " Title: ", \
        jsondata["errors"]["title"], "Detail: ", jsondata["errors"]["detail"]

def makefavourite(number, favourite, connection):
    """ Mark a number resource as favourite """
    body = json.dumps(
        {'data' : {'type' : 'numbers', 'id' : number,
        'attributes' : {'favourite' : favourite}}})
    connection = httplib.HTTPConnection('localhost', 8080)
    connection.request('PATCH', '/numbers/' + number, body)
    response = connection.getresponse()
    connection.close()

def outputnumber(number, connection):
    """ Query a single number resource and print its values """
    connection = httplib.HTTPConnection('localhost', 8080)
    connection.request('GET', '/numbers/' + number)
    response = connection.getresponse()
    if response.status == 404:
        print "Not found!"
    elif response.status == 200:
        body = response.read()
        jsondata = json.loads(body)
        data = jsondata["data"]
        printnumber(data)
    else:
        print "Unexpected server reply: ", response.status, response.reason
    connection.close()

def outputpage(page, pagesize, connection):
    """ Retrieve the page number of the given size """
    connection = httplib.HTTPConnection('localhost', 8080)
    url = '/numbers?page[number]=' + page + '&page[size]=' + pagesize
    connection.request('GET', url)
    response = connection.getresponse()
    if response.status == 404:
        print "Not found!"
    if response.status == 400:
        body = response.read()
        jsondata = json.loads(body)
        printerrorjson(jsondata)
    elif response.status == 200:
        body = response.read()
        jsondata = json.loads(body)
        data = jsondata["data"]
        for number in data:
            printnumber(number)
    else:
        print "Unexpected server reply: ", response.status, response.reason
    connection.close()

def main():
    args = parseargs()
    connection = httplib.HTTPConnection('localhost', 8080)
    if args.number:
        outputnumber(args.number, connection)
    if args.page:
        outputpage(args.page[0], args.page[1], connection)
    if args.favourite:
        makefavourite(args.favourite, 'true', connection)
    if args.unfavourite:
        makefavourite(args.unfavourite, 'false', connection)
    connection.close()

if __name__ == "__main__":
    main()
