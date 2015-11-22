var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var Post = mongoose.model('Post');
var Error = require('../lib/error');

// GET AVAILABLE HASHTAGS
/**
 * @api {get} /hashtags Get Available Hashtags
 * @apiName GetAvailableHashtags
 * @apiVersion 2.0.0
 * @apiGroup Hashtags
 * @apiDescription Gets a list of all hashtags published in Prizm posts with an
 *  optional filter and limit.
 * @apiParam (Query) {String} [filter] An optional filter to determine which 
 *  tags are returned
 * @apiParam (Query) {Number} [limit=10] Sets the number of possible results 
 *  returned
 * @apiParam (Query) {String=tags_only, counts} [format='tags_only'] Format
 *  for hashtag data.
 * @apiParam (Query) {Number} [skip=0] Number of records to skip before the 
 *  returned data.
 * @apiSuccess {String[]} body An array of matched hashtag listed by number of
 *  occurences.
 * @apiSuccessExample Tags only
 *  HTTP/1.1 200 OK
 *  [
 *    "bepassionate",
 *    "bepersistant",
 *    "bepositive",
 *    "beprizmatic"
 *  ]
 * @apiSuccessExample Counts
 *  HTTP/1.1 200 OK
 *  [
 *    {
 *      "tag": "bepassionate",
 *      "count": 45
 *    },
 *    {
 *      "tag": "bepersistant",
 *      "count": 30
 *    },
 *    {
 *      "tag": "bepositive",
 *      "count": 15
 *    },
 *    {
 *      "tag": "beprizmatic",
 *      "count": 8
 *    }
 *  ]
 * @apiUse Error
 **/
app.get('/', function(req, res) {
  var filter = req.query.filter || false;
  var limit = req.query.limit?Number(req.query.limit):10;
  var format = req.query.format || 'tags_only';
  var allowed_formats = ['tags_only', 'counts'];

  var skip = Number(req.query.skip) || 0;
  format = _.contains(allowed_formats, format)?format:'tags_only';
  Post.readHashTags(filter, limit, skip, format, function(err, tags) {
    if (err) Error.serverError(res);
    else res.status(200).json(tags);
  });

});

module.exports = app;
