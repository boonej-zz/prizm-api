/**
 * Activity Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _utils        = require(process.env.PRISM_HOME + 'utils'),
    User          = require(process.env.PRISM_HOME + 'models/user').User,
    Trust         = require(process.env.PRISM_HOME + 'models/user').Trust,
    Post          = require(process.env.PRISM_HOME + 'models/post').Post,
    _object_id    = _mongoose.Schema.Types.ObjectId;
var _ = require('underscore');

/**
 * Activity Model Schema
 * @type {Mongoose.Schema}
 */
var activitySchema = new _mongoose.Schema({
  from:             {type: _object_id, ref: 'User', required: true},
  to:               {type: _object_id, ref: 'User', required: true},
  create_date:      {type: Date, required: false},
  action:           {type: String, default: null, required: false},
  post_id:          {type: String, default: null, required: false},
  comment_id:       {type: String, default: null, required: false},
  insight_id:       {type: String, default: null, required: false},
  insight_target_id: {type: String, default: null, required: false},
  has_been_viewed:  {type: Boolean, default: false, required: false},
  group_id:         {type: _object_id, ref: 'Group', required: false},
  message_id:       {type: _object_id, ref: 'Message', required: false}
}, { versionKey: false });

var baseFields = {from: 1, to: 1, create_date: 1, action: 1, post_id: 1, comment_id: 1,
  insight_target_id: 1, has_been_viewed: 1, group_id: 1};

var baseUserFields = {_id: 1, profile_photo_url: 1, type: 1, name: 1, subtype: 1};

var basePostFields = {_id: 1, text: 1, file_path: 1};


activitySchema.statics.selectFields = function(type){
  if(type === 'short' || type == 'basic'){
    return ['id','from','to','create_date','post_id','comment_id','action', 
      'has_been_viewed', 'insight_id', 'insight_target_id', 'message_id'];
  }
};

activitySchema.statics.canResolve = function(){
  return [
    {from: {identifier: '_id', model: 'User'}},
    {to: {identifier: '_id', model: 'User'}},
    {post_id: {identifier: '_id', model: 'Post'}},
    {comment_id: {identifier: 'comments._id', model: 'Post'}},
    {insight_id: {identifier: '_id', model: 'Insight'}},
    {insight_target_id: {identifier: '_id', model: 'InsightTarget'}},
    {message_id: {identifier: '_id', model: 'Message'}}
  ];
};

activitySchema.methods.format = function(type){
  if(type === 'short' || type === 'basic'){
    return {
      _id: this._id,
      from: this.from,
      to: this.to,
      action: this.action,
      post_id: this.post_id,
      insight_id: this.insight_id,
      insight_target_id: this.insight_target_id,
      comment_id: this.comment_id,
      create_date: this.create_date,
      has_been_viewed: this.has_been_viewed,
      message_id: this.message_id
    };
  }
};

activitySchema.statics.fetchActivitiesForUser = function(uid, last, next) {
  var model = this.model('Activity');
  var criteria = {to: uid};
  if (last) {
    criteria.create_date = {$lt: last};
  }
  model.find(criteria)
  .select(baseFields)
  .sort({create_date: -1})
  .limit(10)
  .populate({path: 'from', model: 'User', select: baseUserFields})
  .populate({path: 'post_id', model: 'Post', select: basePostFields})
  .populate({path: 'insight_id', model: 'Insight', select: {_id: 1, file_path: 1}})
  .populate({path: 'insight_target_id', model: 'InsightTarget', select: {_id: 1, insight: 1}})
  .populate({path: 'group_id', model: 'Group', select: {_id: 1, name: 1}})
  .exec(function(err, activities){
    model.populate(activities, {path: 'insight_target_id.insight', model: 'Insight', select: {_id: 1, file_path: 1}}, 
      function(err, activities){
      next(err, flatten(activities));
      _.each(activities, function(a){
        model.findOneAndUpdate({_id: a._id}, {has_been_viewed: true}, function(err, r){
          if (err) console.log(err);
        });
      });
    });
  });
  
};

var flatten = function(activities) {
  var result = [];
  if (!_.isArray(activities)){
    return null;
  };
  _.each(activities, function(a){
    a = a.toObject();
    if (a.from) {
      a.from_id = a.from._id;
      a.from_subtype = a.from.subtype;
      a.from_profile_photo_url = a.from.profile_photo_url;
      a.from_name = a.from.name;
      a.from_type = a.from.type;
      delete a.from;
    }
    if (a.post_id) {
      var post = a.post_id;
      a.post_id =  post._id || null;
      a.post_file_path = post.file_path || null;
      a.post_text = post.text || null;
    }
    if (a.insight_target_id) {
      var insight = a.insight_target_id;
      a.insight_target_id = insight._id || null;
      a.insight_id = insight.insight._id || null;
      a.insight_file_path  = insight.insight.file_path || null;
    }
    if (a.group_id) {
      var group = a.group_id;
      a.group_id = group._id || null;
      a.group_name = group.name || null;
    }
    result.push(a);
  });
  return result;
}

/**
 * Pre Save Injection/Validation
 *
 * @type {Mongoose.Schema.HookMethod}
 * @param {Function} next Calls the next() iterator function block
 */
activitySchema.pre('save', function(next){
  if (!this.create_date){
    this.create_date = new Date();
  }
  next();
});

exports.Activity = _mongoose.model('Activity', activitySchema);

