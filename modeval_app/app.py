from flask import Flask, redirect, url_for

app = Flask(__name__)

@app.route("/")
def index():
    return('Hello World')

if __name__ == '__main__':
    app.run(debug = True)
