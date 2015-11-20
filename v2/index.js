var express = require('express');
var app = express();
var organizations = require('./organizations');
var users = require('./users');
var interests = require('./interests');
var posts = require('./posts');

/**
 * @apiDefine Error
 * @apiError (Error 400) invalid_request The request was malformed or 
 *  otherwise unusable.
 * @apiErrorExample Bad Request:
 *  HTTP/1.1 400 Bad Request
 *  {
 *    "error": "invalid_request",
 *    "description": "You must specify a user id."
 *  }
 * @apiError (Error 500) server_error There was a server side processing
 *  error during the request.
 * @apiErrorExample Server Error:
 *  HTTP/1.1 500 Internal Server Error
 *  {
 *    "error": "server_error",
 *    "description": "There was a problem processing your request."
 *  }
**/

app.use('/organizations/', organizations); 
app.use('/users/', users);
app.use('/interests/', interests);
app.use('/posts/', posts);
app.get('/', function(req, res){
  res.send('Welcome to the V2 API');
});

module.exports = app;
