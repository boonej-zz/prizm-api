var express = require('express');
var app = express();
var organizations = require('./organizations');

app.use('/organizations/', organizations); 
app.get('/', function(req, res){
  res.send('Welcome to the V2 API');
});

module.exports = app;
