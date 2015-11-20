var express = require('express');
var app = express();
var organizations = require('./organizations');
var users = require('./users');
var interests = require('./interests');
var posts = require('./posts');

/**
 * @apiDefine ErrorInvalid
 * @apiError (Error 400) error Short description of error.
 * @apiError (Error 400) description Descriptive error text.
**/

/**
 * @apiDefine ErrorServer
 * @apiError (Error 500) error Short description of error.
 * @apiError (Error 500) description Descriptive error text.
**/

/**
 * @apiDefine Error
 * @apiError (Error 400) error Short description of error.
 * @apiError (Error 400) description Descriptive error text.
 * @apiError (Error 500) error Short description of error.
 * @apiError (Error 500) description Descriptive error text.
**/

app.use('/organizations/', organizations); 
app.use('/users/', users);
app.use('/interests/', interests);
app.use('/posts/', posts);
app.get('/', function(req, res){
  res.send('Welcome to the V2 API');
});

module.exports = app;
