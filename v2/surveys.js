var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var error = require('../lib/error');
var Survey = mongoose.model('Survey');
var Question = mongoose.model('Question');
var Answer = mongoose.model('Answer');

app.get('/:sid', function(req, res) {

  var sid = req.params.sid;
  var q = req.query.q || 1;
  
  Survey.fetchSurveyQuestion(sid, q, function(err, survey){
    if (err) Error.serverError(res);
    else res.status(200).json(survey);
  });

});

app.post('/:sid/questions/:qid/answers', function(req, res) {
  
  var sid = req.params.sid;
  var qid = req.params.qid;

  var value = req.body.value;
  var user = req.body.user;

  var q = req.query.next || false;
  var answer;

  Question.findOne({_id: qid})
  .populate({path: 'answers', model: 'Answer'})
  .exec(function(err, question){
    _.each(question.answers, function(a){
      if (String(a.user) == String(user)) {
        answer = a;
      }
    });
    if (answer) {
      answer.value = value;
      answer.save(function(err, a){
      });
    } else {
      answer = new Answer({user: user, value: value});
      console.log(answer);
      console.log(q.order);
      answer.save(function(err, a){
        Question.findOne({_id: qid}, function(err, qq){
          qq.answers.push(a._id);
          qq.save(function(err, qq){

          });
        });
      });
    }
    if (q) {
      Survey.fetchSurveyQuestion(sid, q, function(err, secQ){
        if (err) Server.serverError(res);
        else {
          console.log(secQ);
          res.status(200).json(secQ);
        }
      });
    } else {
      console.log('testing if final question');
      Survey.findOne({_id: sid}, function(err, survey) {
        if (survey.number_of_questions == question.order) {
          console.log('finalizing survey');
          var present = false;
          _.each(survey.completed, function(u) {
            if (String(u) === String(user)) {
              present = true;
            }
          });
          if (!present) {
            survey.completed.push(user);
          }
          survey.save(function(err, s){
            if (err) console.log(err);
          });
        
        }
        res.status(200).json({complete: true});
      });

    }
  });

  

});


module.exports = app;
