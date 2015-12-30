var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var error = require('../lib/error');
var Insight = mongoose.model('Insight');

/**
 * @api {get} /insights/:iid Get Insight
 * @apiName GetInsight
 * @apiVersion 2.0.0
 * @apiGroup Insights
 * @apiDescription Retrieves the content of a single insight.
 * @apiParam {String} iid Unique identifier for insight
 * @apiUse Error
 **/
app.get('/:iid', function(req, res){
  var iid = req.params.iid;
  if (iid) {
    Insight.fetchInsight(iid, function(err, insight){
      if (err) {
        Error.serverError(res);
      } else {
        res.status(200).json(insight);
      }
    });
  } else {
    Error.invalidRequest(res, 'You must provide a unique identifier');
  }
});

module.exports = app;
