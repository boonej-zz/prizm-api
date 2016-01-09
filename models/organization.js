var mongoose          = require('mongoose'),
    utils             = require(process.env.PRISM_HOME + 'utils'),
    User              = mongoose.model('User'),
    ObjectId          = mongoose.Schema.Types.ObjectId;
var mObjectId         = mongoose.Types.ObjectId;
var _ = require('underscore');
var moment = require('moment');

var organizationSchema = new mongoose.Schema({
  code                : {type: String, default: null, required: true, 
                          index: true},
  theme               : {type: ObjectId, ref: 'Theme', required: false},
  name                : {type: String, default: null, required: true},
  create_date         : {type: Date, default: null, required: false},
  modify_date         : {type: Date, default: null, required: false},
  members             : {type: Array, default: []},
  welcome_image_url   : {type: String, default: null},
  logo_url            : {type: String, default: null},
  owner               : {type: ObjectId, ref: 'User', required: false},
  namespace           : {type: String, required: false},
  stripe_id           : {type: String, default: null},
  groups              : {type: Array, default: []},
  mutes               : {type: Array, default: []},
  who_to_follow       : {
    luminaries: {type: Boolean, default: true},
    org_luminaries: {type: Boolean, default: false},
    leaders: {type: Boolean, default: false},
    ambassadors: {type: Boolean, default: false}
  },
  featured            : {
    partners: {type: Boolean, default: false},
    ambassadors: {type: Boolean, default: false},
    luminaries: {type: Boolean, default: false},
    leaders: {type: Boolean, default: false}
  }
});

organizationSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

organizationSchema.statics.selectFields = function(type){
  var select = ['id', 'code', 'theme', 'name', 'create_date', 'modify_date',
      'logo_url', 'welcome_image_url', 'groups', 'mutes', 'owner'];
  return select;
};

organizationSchema.methods.format = function(type, add_fields){
  format = {
    _id:          this._id,
    code:         this.code,
    create_date:  this.create_date,
    modify_date:  this.modify_date,
    name:         this.name,
    theme:        this.theme,
    welcome_image_url: this.welcome_image_url,
    logo_url        : this.logo_url,
    groups          : this.groups,
    mutes           : this.mutes,
    owner           : this.owner
  };
  return format;
}

organizationSchema.methods.fetchGroups = function(next) {
  var model = this.model('Organization');
  model.populate(this, {path: 'groups', model: 'Group'}, function(err, organization){
    model.populate(organization, {path:'groups.leader', model: 'User'}, function(err, organization){
      var groups = false;
      var result = [];
      if (organization.groups) {
        groups = _.filter(organization.groups, function(obj) {
          return obj.status == 'active';
        });
        _.each(groups, function(g) {
          g = g.toObject();
          if (g.leader) {
            g.leader_name = g.leader.name;
          }
          result.push(g);
        });
      }
      next(err, result); 
    });
  });
};
organizationSchema.statics.canResolve = function(){
  return [
    {members: {identifier: '_id', model: 'User'}},
    {theme: {identifier: 'code', model: 'Theme'}},
    {groups: {identifier: '_id', model: 'Group'}},
    {mutes: {identifier: '_id', model: 'Mute'}},
    {owner: {identifier: '_id', model: 'User'}}
  ];
}

organizationSchema.statics.findOneAndFlatten = function(oid, next) {
  var model = this.model('Organization');
  var User = this.model('User');
  model.findOne({_id: oid}).
    exec(function(err, organization){
      User.count(
        {
          active: true,
          org_status: {
            $elemMatch: {
              organization: organization._id,
              status: 'active'
            }
          }
        }, function(err, count){
          var result = organization.toObject();
          result.member_count = count;
          next(err, result);
        });
    });
}


organizationSchema.statics.mute = function(oid, uid, next) {
  var model = this.model('Organization');
  model.findOne({_id: oid}, function(err, org) {
    if (org) {
      var exists = false;
      if (!org.mutes) {
        org.mutes = [];
      }
      _.each(org.mutes, function(mute){
        if (String(mute) == String(uid)) {
          exists = true;
        }
      });
      if (!exists) {
        org.mutes.push(uid);
        org.save(function(err, o){
          if (err) console.log(err);
        });
      }
      org = org.toObject();
      org.muted = true;
    }
    
    next(err, org);
  });
}

organizationSchema.statics.unmute = function(oid, uid, next) {
  var model = this.model('Organization');
  model.findOne({_id: oid}, function(err, org) {
    if (org) {
      var idx = -1;
      if (!org.mutes) {
        org.mutes = [];
      }
      _.each(org.mutes, function(mute, i){
        if (String(mute) == String(uid)) {
          idx = i;
        }
      });
      if (idx != -1) {
        org.mutes.splice(idx, 1);
        org.save(function(err, o){
          if (err) console.log(err);
        });
      }
      org = org.toObject();
      org.muted = false;
    }
    next(err, org);
  });
};

var userProps = {_id: 1, first_name: 1, last_name: 1, name: 1, profile_photo_url: 1, active: 1, type: 1, subtype: 1};

organizationSchema.statics.fetchLeaderboard = function(oid, limit, skip, next){
  var model = this.model('Organization');
  var User = mongoose.model('User');
  var Survey = mongoose.model('Survey');
  var surveyPoints = {};
  User.find({active: true, org_status: {$elemMatch: {status: 'active', organization: mObjectId(oid)}}})
  .select({_id: 1})
  .exec(function(err, users){
    console.log(users);
    var u = _.pluck(users, '_id');
    Survey.find({completed: {$in: u}, organization: oid})
    .populate({path: 'questions', model: 'Question'})
    .populate({path: 'completed', model: 'User', select: userProps})
    .exec(function(err, surveys){
      Survey.populate(surveys, {path: 'questions.answers', model: 'Answer'}, function(err, surveys){
        _.each(surveys, function(survey){
          _.each(survey.completed, function(u){
            if (! surveyPoints[String(u._id)]) {
              var key = String(u._id);
              surveyPoints[key] = {user: u, points: (survey.number_of_questions * 10), surveys: 1};

            } else {
              surveyPoints[String(u._id)].points += (survey.number_of_questions) * 10;
              surveyPoints[String(u._id)].surveys += 1;
            }
          });
          _.each(survey.questions[0].answers, function(a){
            if (surveyPoints[String(a.user)]) {
              surveyPoints[String(a.user)].startTime = a.create_date;
            }
          });
          _.each(survey.questions[survey.questions.length -1].answers, function(a){
            var q = survey.questions[survey.questions.length - 1];
            if (surveyPoints[String(a.user)]) {
              surveyPoints[String(a.user)].endTime = a.create_date;
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
        model.findOne({_id: oid})
        .select({namespace: 1})
        .exec(function(err, org){
          var response = [];
          for (key in surveyPoints) {
            var item = surveyPoints[key];
            var obj = {
              user_id: item.user._id,
              user_first_name: item.user.first_name,
              user_last_name: item.user.last_name,
              user_name: item.user.name,
              user_type: item.user.type,
              user_subtype: item.user.subtype,
              user_active: item.user.active,
              user_profile_photo_url: item.user.profile_photo_url,
              survey_count: item.surveys,
              organization_namespace: org.namespace,
              points: Math.round(item.points)
            };
            response.push(obj);
          }
          response = _.sortBy(response, function(item){
            return -item.points;
          });
          _.each(response, function(item, idx){
            var position = skip + idx;
            switch (position) {
              case 0:
                item.rank = '1st';
                break;
              case 1:
                item.rank = '2nd';
                break;
              case 2:
                item.rank = '3rd';
                break;
              default:
                item.rank = idx + 'th';
                break;
            }
          });
          if (limit && skip) {
            if (limit - skip < response.length) {
              response = response.slice(skip, limit); 
            }
          }
          next(err, response);

        });
      });
    }); 
  });

};

organizationSchema.statics.fetchIndividualScore = function(oid, uid, next) {
  var model = this.model('Organization');
  model.fetchLeaderboard(oid, false, false, function(err, users) {
    var user;
    if (users) {
      _.each(users, function(u, idx){
        if (String(u.user_id) == String(uid)) {
          user = u;
        }
      });
    }
    next(err, user);
  });
};


var themeSchema = new mongoose.Schema({
  background_url      : {type: String, default: null},
  dominant_color      : {type: String, default: null},
  text_color          : {type: String, default: null},
  create_date         : {type: Date, default: null},
  modify_date         : {type: Date, default: null},
  organization        : {type: ObjectId, ref: 'Organization', required: true}
});

themeSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

themeSchema.statics.selectFields = function(type){
  return ['_id', 'background_url', 'dominant_color', 'text_color', 
         'create_date', 'modify_date', 'organization'];
};

themeSchema.methods.format = function(type, add_fields, callback){
  return {
    _id:              this._id,
    background_url:   this.background_url,
    dominant_color:   this.dominant_color,
    text_color:       this.text_color,
    create_date:      this.create_date,
    modify_date:      this.modify_date,
    organization:     this.organization
  };
};

themeSchema.statics.canResolve = function(){
  return [{organization: {identifier: '_id', model: 'Organization'}}];
};


exports.Organization = mongoose.model('Organization', organizationSchema);
exports.Theme = mongoose.model('Theme', themeSchema);
