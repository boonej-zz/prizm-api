var mongoose = require('mongoose');
var ObjectId = mongoose.Schema.Types.ObjectId;
var _ = require('underscore');
var iPush = require('../classes/i_push');
var moment = require('moment');

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

surveySchema.statics.fetchLatestSurveyCompletionData = function(oid, next) {

  var model = this.model('Survey');

  model.findOne({organization: oid, status: 'active', targeted_users: {$ne: []}})
  .sort({create_date: -1})
  .populate({path: 'questions', model: 'Question'})
  .exec(function(err, survey){
    model.populate(survey, {path: 'questions.answers', model: 'Answer'}, 
      function(err, survey){
        var answers = survey.questions[survey.questions.length - 1].answers;
        var results = [];
        if (answers.length > 0) {
          answers = _.sortBy(answers, function(answer) {
            return -answer.create_date;
          });

          var startDate = answers[0].create_date;
          var startDay = answers[0].create_date.getDate();
          var startMonth = answers[0].create_date.getMonth() + 1;

          for (var i = 0; i != 3; ++i) {
            var compDate = new Date(startDate.getTime());
            compDate.setDate(startDate.getDate() - i);
            var dateString = compDate.getDate();
            var monthString = compDate.getMonth() + 1;
            var key = monthString + '/' + dateString;
            var item = {date: key, count: 0};
            results.push(item); 
            _.each(answers, function(answer) {
              if (answer.create_date.getDate() == compDate.getDate() &&
                answer.create_date.getMonth() == compDate.getMonth()){
                results[i].count += 1;  
              } 
            }); 
          }
          
        }
        results.reverse();
        next(err, results);
      });
  });
}

var userParams = {_id: 1, first_name: 1, last_name: 1, name: 1, type: 1, 
  subtype: 1, profile_photo_url: 1};

surveySchema.statics.fetchRespondants = function(oid, next){
  
  var model = this.model('Survey');
  var User = mongoose.model('User');

  model.find({organization: oid })
  .sort({create_date: -1})
  .limit(1)
  .populate({path: 'questions', model: 'Question'})
  .exec(function(err, surveys){
    if (surveys && surveys.length > 0) {
    var survey = surveys[0];
    model.populate(survey, {path: 'questions.answers', model: 'Answer'}, 
      function(err, survey){
      model.populate(survey, {path: 'targeted_users.user', model: 'User', 
        select: userParams}, function(err, survey){
         var users = [];
         _.each(survey.targeted_users, function(u){
            u = u.toObject();
            var uObj = u.user;
            uObj.invite_date = u.create_date;
            uObj.completed = false;
            for (var i = 0; i != survey.completed.length; ++i) {
              var item = String(survey.completed[i]);
              if (String(uObj._id) == item) {
                uObj.completed = true;
                break;
              }
            }
            if (uObj.completed) {
              var startTime;
              var endTime;
              _.each(survey.questions[0].answers, function(a){
                if (String(a.user) == String(uObj._id)) {
                  startTime = a.create_date;
                }
              });
              _.each(survey.questions[survey.questions.length - 1].answers, function(a){
                if (String(a.user) == String(uObj._id)) {
                  endTime = a.create_date;
                }
              });
              uObj.duration = moment.utc(moment.duration(moment(endTime).subtract(startTime)).asMilliseconds()).format('HH:mm:ss');
            } else {
              uObj.duration = "";
            }
            users.push(uObj);
         });
         next(err, users);
      }); 
    });
    } else {
      next(err, []);
    }
  });
};

mongoose.model('Answer', answerSchema);
mongoose.model('Question', questionSchema);
mongoose.model('Survey', surveySchema);
