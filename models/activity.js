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
  type:         {type: String, default: null, required: true},
  user:         {type: _object_id, ref: 'User', required: true},
  target:       {type: _object_id, ref: 'User', required: true},
  create_date:  {type: Date, default: null, required: false},
  scope:        {type: String, default: 'public', required: false},
  object:       {type: Object, default: null, required: false},
  context:      {type: Object, default: null, required: false},
  route:        {type: String, default: null, required: false},
  action:       {type: String, default: null, required: false}
}, { versionKey: false });

activitySchema.statics.selectFields = function(type){
  if(type === 'short'){

  }

};

/**
 * Pre Save Injection/Validation
 *
 * @type {Mongoose.Schema.HookMethod}
 * @param {Function} next Calls the next() iterator function block
 */
activitySchema.pre('save', function(next){
  if(!this.create_date){
    this.create_date = new Date();
  }
  next();
});

exports.Activity = _mongoose.model('Activity', activitySchema);

