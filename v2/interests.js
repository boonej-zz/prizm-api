var express = require('express');
var app = express();
var gateway = require('../gateway');
var mongoose = require('mongoose');
var Interest = mongoose.model('Interest');

app.get('/', gateway, function(req, res) {
  console.log('in request');
  Interest.find({})
  .exec(function(err, interests){
    if (err) res.status(500).json(err);
    else res.status(200).json(interests);
  });
});

module.exports = app;
