var mongoose = require('mongoose');
var ObjectId = mongoose.Schema.Types.ObjectId;
var _ = require('underscore');
var iPush = require('../classes/i_push');

var answerSchema = new mongoose.Schema({
  user: {type: ObjectId, ref: 'User', required: true},
  value: {type: Number, required: true},
  create_date: {type: Date}
});

answerSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  next();
});

var questionSchema = new mongoose.Schema({
  text: {type: String, required: true},
  type: {type: String, required: true},
  values: [{question: {type: String}, order: {type: Number}}],
  scale: {type: Number},
  create_date: {type: Date},
  modify_date: {type: Date},
  order: {type: Number},
  answers: [ObjectId]
});

questionSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

var surveySchema = new mongoose.Schema({
  status: {type: String, default: 'active'},
  name: {type: String, required: true},
  creator: {type: ObjectId, ref: 'User', required: true},
  create_date: {type: Date},
  modify_date: {type: Date},
  organization: {type: ObjectId, ref: 'Organization', required: true},
  groups: [ObjectId],
  number_of_questions: {type: Number},
  questions: [ObjectId],
  completed: [ObjectId],
  targeted_users: [{user: {type: ObjectId, ref: 'User'}, create_date: {type: Date}}],
  target_all: {type: Boolean, default: false}
});

surveySchema.pre('save', function(next){
  if (!this.create_date){
    this.create_date = Date.now();
  }
  if (!this.status) {
    this.status = 'active';
  }
  this.modify_date = Date.now();
  next();
});

surveySchema.methods.notifyUsers = function(users, next){
  var model = this.model('Survey');
  model.populate(
    this,
    {path: 'targeted_users.user', model: 'User'},
    function(err, survey) {
      model.populate(survey, {path: 'creator', model: 'User'}, function(err, survey){
      if (survey) {
        var notified = _.filter(survey.targeted_users, function(obj){
          if (users) {
            var valid = false;
            _.each(users, function(u){
              if (String(u) == String(obj.user._id)) {
                valid = true;
              }
            });
            return valid;
          } else {
            return true;
          }
        });
        notified = _.filter(notified, function(obj){
          var valid = true;
          _.each(survey.completed, function (c){
            if (String(c) == String(obj.user._id)){
              valid = false;
            }
          });
          return(valid);
        });
        console.log('Sending to ' + notified);
        var messageString = survey.creator.name + ' has sent you a new survey.'; 
        _.each(notified, function(u){
          iPush.sendNotification({
            device: u.device_token,
            alert: messageString,
            payload: {survey: survey._id},
            badge: 1
          }, function(err, result){
            if (err) console.log(err);
            else console.log('Sent push');
          }); 
        });
        next(err, survey);
      } else {
        console.log(err);
        next(err, false);
      }
    }
  );
});
}; 

surveySchema.statics.findOneAndNotify = function(params, users, next){
  this.findOne(params, function(err, survey){
    if (!survey) {
      if (err) console.log(err);
      next(err, false);
    } else {
      survey.notifyUsers(users, next);
    }
  });
}

mongoose.model('Answer', answerSchema);
mongoose.model('Question', questionSchema);
mongoose.model('Survey', surveySchema);
