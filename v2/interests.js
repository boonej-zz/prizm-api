var express = require('express');
var app = express();
var gateway = require('../gateway');
var mongoose = require('mongoose');
var Interest = mongoose.model('Interest');

/**
 * @api {get} /interests Get Interests
 * @apiName GetInterests
 * @apiDescription Gets a list of all available interests.
 * @apiVersion 2.0.0
 * @apiGroup Interests
 * @apiUse Error
 **/
app.get('/', gateway, function(req, res) {
  console.log('in request');
  Interest.find({})
  .exec(function(err, interests){
    if (err) res.status(500).json(err);
    else res.status(200).json(interests);
  });
});

module.exports = app;
