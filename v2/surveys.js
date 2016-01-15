var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var error = require('../lib/error');
var Survey = mongoose.model('Survey');

app.get('/:sid', function(req, res) {

  var sid = req.params.sid;
  var q = req.query.q || 1;
  
  Survey.fetchSurveyQuestion(sid, q, function(err, survey){
    if (err) Error.serverError(res);
    else res.status(200).json(survey);
  });

});


module.exports = app;
