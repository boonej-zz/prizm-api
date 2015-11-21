var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var Post = mongoose.model('Post');
var Error = require('../lib/error');

// GET AVAILABLE HASHTAGS
/**
 * @api {get} / Get Available Hashtags
 * @apiName GetAvailableHashtags
 * @apiVersion 2.0.0
 * @apiGroup Hashtags
 * @apiDescription Gets a list of all hashtags published in Prizm posts with an
 *  optional filter and limit.
 * @apiParam (Query) {String} [filter] An optional filter to determine which 
 *  tags are returned
 * @apiParam (Query) {Number} [limit=10] Sets the number of possible results 
 *  returned
 * @apiSuccess {String[]} body An array of matched hashtag listed by number of
 *  occurences.
 * @apiSuccessExample Success Response
 *  HTTP/1.1 200 OK
 *  [
 *    "bepassionate",
 *    "bepersistant",
 *    "bepositive",
 *    "beprizmatic"
 *  ]
 * @apiUse Error
 **/
app.get('/', function(req, res) {
  var filter = req.query.filter || false;
  var limit = req.query.limit?Number(req.query.limit):10;
  Post.readHashTags(filter, limit, function(err, tags) {
    if (err) Error.serverError();
    else res.status(200).json(tags);
  });

});

module.exports = app;
