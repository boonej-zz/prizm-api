var mongoose   = require('mongoose');
var prism_home = process.env.PRISM_HOME;
var utils      = require(prism_home + 'utils');
var logger     = require(prism_home + 'logs.js');
var PrismError  = require(prism_home + 'error');
var User        = mongoose.model('User');
var Twine       = require(prism_home + 'classes/Twine');
var _ = require('underscore');
var ObjectId    = require('mongoose').Types.ObjectId;
var Survey = mongoose.model('Survey');
var Question = mongoose.model('Question');
var Answer = mongoose.model('Answer');
var moment = require('moment');

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
  var value = req.body.answer_value;
  console.log(uid);
  console.log(value);
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
        answer.save(function(err, answer){
          if (err) console.log(err);
          q.answers.push(answer);
            q.markModified('answers');
            q.save(function(err, result){
            utils.prismResponse(res, result, true);
          });
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
        Survey.populate(result, {path: 'questions', model: 'Question'}, function(err, survey){
          Survey.populate(survey, {path: 'questions.answers', model: 'Answer'}, function(err, survey){
            var startTime;
            var endTime;
            survey = survey.toObject();
            survey.rank = survey.completed.length;
            _.each(survey.questions[0].answers, function(a){
              if (String(a.user) == String(uid)){
                startTime = a.create_date;
              }
            });
            _.each(survey.questions[survey.questions.length - 1].answers, function(a){
              if (String(a.user) == String(uid)){
                endTime = a.create_date;
              }
            });
            survey.duration = moment.utc(moment.duration(moment(endTime).subtract(startTime)).asMilliseconds()).format('HH:mm:ss');
            utils.prismResponse(res, survey, true);
          });
        });
      });
    } else {
      if (err) console.log(err);
      res.status(400).send();
    }
  });
}

exports.getLeaderboard = function(req, res) {
  var oid = req.params.oid;

  var surveyPoints = {};
  User.find({active: true, org_status: {$elemMatch: {status: 'active', organization:ObjectId(oid)}}})
  .exec(function(err, users){
    var u = _.pluck(users, '_id');
    Survey.find({completed:  {$in: u}, organization: oid})
    .populate({path: 'questions', model: 'Question'})
    .populate({path: 'completed', model: 'User', select: {_id: 1, first_name: 1, last_name: 1, name: 1,  profile_photo_url: 1, type: 1, subtype: 1, active: 1}})
    .exec(function(err, surveys){
      Survey.populate(surveys, {path: 'questions.answers', model: 'Answer'}, function(err, surveys){
        _.each(surveys, function(survey){
          _.each(survey.completed, function(u){
            if (! surveyPoints[String(u._id)]) {
              var key = String(u._id);
              surveyPoints[key] = {user: u, points: (survey.number_of_questions * 10), surveys: 1};
            } else {
              surveyPoints[String(u._id)].points += (survey.number_of_questions * 10);
              surveyPoints[String(u._id)].surveys += 1;
            }
          });
          _.each(survey.questions[0].answers, function(a){
            if (surveyPoints[String(a.user)]) { 
              surveyPoints[String(a.user)].startTime = a.create_date;
            }
          });
          _.each(survey.questions[survey.questions.length - 1].answers, function(a){
            var q = survey.questions[survey.questions.length -1];
            if (surveyPoints[String(a.user)]) {
            surveyPoints[String(a.user)].endTime = a.create_date;
            console.log(a.user);
            console.log(surveyPoints);
            var take_diff = moment.duration(new moment(a.create_date).diff(surveyPoints[String(a.user)].startTime));
            var take_hours = Math.round(take_diff.asHours()/6);
            var speed_points = (25 - (take_hours * 5));
            speed_points = speed_points > 0?speed_points:0;
            var respond_diff = moment.duration(new moment(a.create_date).diff(q.create_date));
            var respond_hours = respond_diff.asMinutes();
            response_points = (25 - (respond_hours *5));
            response_points = response_points > 0?response_points:0;
            surveyPoints[String(a.user)].points += (speed_points + response_points);
            }
          });
        });
        var response = [];
        for (key in surveyPoints){
          response.push({
            _id: surveyPoints[key].user._id,
            user: surveyPoints[key].user,
            points: Math.round(surveyPoints[key].points),
            organization: oid,
            surveys: surveyPoints[key].surveys
          });
        }
        utils.prismResponse(res, response, true);
      });
    });
  });
}

exports.getUserSurveys = function(req, res) {
  var uid = req.params.uid;
  Survey.find({completed: uid})
  .populate({path: 'questions', model: 'Question'})
  .exec(function(err, surveys){
    Survey.populate(surveys, {path: 'questions.answers', model: 'Answer', select: {create_date: 1, user: 1}}, function(err, surveys){
      if (err) console.log(err);
      var returnSurveys = [];
      _.each(surveys, function(survey) {
        survey = survey.toObject();
        survey.points = (survey.number_of_questions * 10);
        var points = {};
        _.each(survey.completed, function(user) {
          points[String(user)] = {user: user, points: (survey.number_of_questions * 10)};
        });
        console.log(points);
        _.each(survey.questions[0].answers, function(a) {
          if (points[String(a.user)]) {
            points[String(a.user)].startTime = a.create_date;
          }
        });
        _.each(survey.questions[survey.questions.length - 1].answers, function(a) {
          if (points[String(a.user)]) {
            var take_diff = moment.duration(new moment(a.create_date).diff(points[String(a.user)].startTime));
            var take_hours = Math.round(take_diff.asHours()/6);
            var speed_points = (25 - (take_hours * 5));
            speed_points = speed_points > 0?speed_points:0;
            var respond_diff = moment.duration(new moment(a.create_date).diff(survey.create_date));
            var respond_hours = respond_diff.asMinutes();
            var response_points = (25 - (respond_hours *5));
            response_points = response_points > 0?response_points:0;
            points[String(a.user)].points += (speed_points + response_points);
            var duration = moment.utc(moment.duration(moment(a.create_date).subtract(points[String(a.user)].startTime)).asMilliseconds()).format('HH:mm:ss');
            points[String(a.user)].duration = duration;
          }
          console.log(points);
        });
        var pointsArray = [];
        for (key in points) {
          pointsArray.push(points[key]);
        }

        pointsArray = _.sortBy(pointsArray, function(item){
          return -item.points;
        });
        for (var i = 0; i < pointsArray.length; ++i) {
          var item = pointsArray[i];
          if (String(item.user) == String(uid)) {
            survey.rank = i + 1;
            survey.points = item.points;
            survey.duration = item.duration;
          }
        }
        returnSurveys.push(survey);
      });
      utils.prismResponse(res, returnSurveys, true);
    });
  });
}

exports.getLatestSurvey = function(req, res) {
  var oid = req.params.oid;
  Survey.findOne({organization: oid, status: 'active'})
  .sort({date_created: -1})
  .populate({path: 'questions', model: 'Question'})
  .populate({path: 'targeted_users', model: 'User', select: {_id: 1, first_name: 1, last_name: 1, name: 1, active: 1, profile_photo_url: 1}})
  .exec(function(err, survey){
    if (err) console.log(err);
    if (survey) {
      Survey.populate(survey, {path: 'questions.answers', model: 'Answer'}, function(err, survey){
        if (err) console.log(err);
        utils.prismResponse(res, survey, true);
      });
    } else {
        res.status(400).send('Bad request');
    }
  });
};
