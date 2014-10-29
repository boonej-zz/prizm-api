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
  has_been_viewed:  {type: Boolean, default: false, required: false}
}, { versionKey: false });

activitySchema.statics.selectFields = function(type){
  if(type === 'short' || type == 'basic'){
    return ['id','from','to','create_date','post_id','comment_id','action', 
      'has_been_viewed', 'insight_id', 'insight_target_id'];
  }
};

activitySchema.statics.canResolve = function(){
  return [
    {from: {identifier: '_id', model: 'User'}},
    {to: {identifier: '_id', model: 'User'}},
    {post_id: {identifier: '_id', model: 'Post'}},
    {comment_id: {identifier: 'comments._id', model: 'Post'}},
    {insight_id: {identifier: '_id', model: 'Insight'}},
    {insight_target_id: {identifier: '_id', model: 'InsightTarget'}}
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
      has_been_viewed: this.has_been_viewed
    };
  }
};

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

