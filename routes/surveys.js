var mongoose   = require('mongoose');
var prism_home = process.env.PRISM_HOME;
var utils      = require(prism_home + 'utils');
var logger     = require(prism_home + 'logs.js');
var PrismError  = require(prism_home + 'error');
var User        = mongoose.model('User');
var Twine       = require(prism_home + 'classes/Twine');
var _ = require('underscore');
var ObjectId    = mongoose.Schema.Types.ObjectId;
var Survey = mongoose.model('Survey');

exports.fetchUserSurveys = function(req, res) {
  var uid = req.params.uid;
  Survey.find({targeted_users: {$elemMatch: {user: uid}}, completed: {$ne: uid}, status: {$ne: 'inactive'}})
  .populate({path: 'creator', model: 'User', select: {_id: 1, name: 1, profile_photo_url: 1}})
  .populate({path: 'questions', model: 'Question'})
  .populate({path: 'answers', model: 'Answer'})
  .populate({path: 'organization', model: 'Organization'})
  .exec(function(err, surveys){
    if (err) {
      console.log(err);
    }
    console.log(surveys);
    utils.prismResponse(res, surveys, true);
  });
};

exports.postAnswer = function(req, res) {
  var uid = req.body.uid;
  var qid = req.params.qid;
  var value = req.body.value;
  Question.findOne({_id: qid}, function(err, q){
    if (q){
      var current = _.filter(q.answers, function(obj){
        return String(obj.user) == String(uid);
      });
      var answer = false;
      if (current.length > 0) {
        answer = current[0];
        answer.value = value;
        answer.create_date = date.now();
        answer.save(function (err, result){ 
          utils.prismResponse(res, q, true);
        });
      } else {
        answer = new Answer({user: uid, value: value});
        q.answers.push(answer);
        q.save(function(err, result){
          utils.prismResponse(res, result, true);
        });
      }
    } else {
      if (err) console.log(err);
      res.status(400).send();
    }
  });
}

exports.finalizeSurvey = function(req, res) {
  var uid = req.body.uid;
  var sid = req.params.sid;
  Survey.findOne({_id: sid}, function(err, survey){
    if (survey) {
      survey.completed.push(uid);
      survey.save(function(err, result){
        utils.prismResponse(res, result, true);
      });
    } else {
      if (err) console.log(err);
      res.status(400).send();
    }
  });
}
