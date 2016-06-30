from flask import Flask, redirect, url_for

app = Flask(__name__)

@app.route("/")
def index():
    # Enable users to upload a dataset
    return('Hello World')

@app.route("map")
def map():
    # Map input values to numeric values
    return('This functionality does not exist (yet).')

@app.route("results", methods = ['GET', 'POST'])
def result():
    # return correlation
    # some plot
    return('results')

if __name__ == '__main__':
    app.run(debug = True)
