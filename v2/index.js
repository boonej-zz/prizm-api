var express = require('express');
var app = express();
var organizations = require('./organizations');
var users = require('./users');
var interests = require('./interests');

app.use('/organizations/', organizations); 
app.use('/users/', users);
app.use('/interests/', interests);
app.get('/', function(req, res){
  res.send('Welcome to the V2 API');
});

module.exports = app;
