/**
 * User Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _serial     = require('serializer'),
    _crypt      = require('crypto'),
    _prism_home = process.env.PRISM_HOME,
    // Activity    = require(process.env.PRISM_HOME + 'models/activity').Activity,
    _utils      = require(_prism_home + 'utils');

var trustSchema = new _mongoose.Schema({
  user_id             : {type: _mongoose.Schema.Types.ObjectId, ref: 'User', required: true},
  status              : {type: String, required: true},
  is_owner            : {type: Boolean, required: true},
  create_date         : {type: Date, default: null},
  modify_date         : {type: Date, default: null},
  delete_date         : {type: Date, default: null, select: false}
},
{
  versionKey          : false
});

trustSchema.path('status').validate(function(value){
  value.toLowerCase();
  value = value.charAt(0).toUpperCase() + value.slice(1);
  return /Accepted|Rejected|Pending|Canceled|Cancelled/i.test(value);
});

trustSchema.pre('save', function(next){
  if(!this.create_date){
    this.create_date = new Date();
  }
   this.modify_date = new Date();
   next();
});

var userSchema = new _mongoose.Schema({
  name                  : {type: String, default: '', select: true},
  first_name            : {type: String, required: true, select: true},
  last_name             : {type: String, required: true, select: true},
  email                 : {type: String, required: true, index: {unique: true}, select: true},
  info                  : {type: String, default: null, select: true},
  website               : {type: String, default: null, select: true},
  ethnicity             : {type: String, default: null, select: false},
  religion              : {type: String, default: null, select: false},
  affiliations          : {type: Array, default:[], select: false},
  password              : {type: String, default: null, select: false},
  provider              : {type: String, default: null, select: false},
  provider_id           : {type: String, default: null, select: false},
  provider_token        : {type: String, default: null, select: false},
  provider_token_secret : {type: String, default: null, select: false},
  last_provider_auth    : {type: Date, default: null, select: false},
  gender                : {type: String, default: null, select: false},
  birthday              : {type: String, default: null, select: false},
  address               : {type: String, default: null, select: false},
  city                  : {type: String, default: null, select: true},
  country               : {type: String, default: null, select: false},
  state                 : {type: String, default: null, select: true},
  zip_postal            : {type: String, default: null, select: false},
  cover_photo_url       : {type: String, default: '', select: true},
  profile_photo_url     : {type: String, default: '', select: true},
  create_date           : {type: Date, default: null, select: true},
  modify_date           : {type: Date, default: null, select: false},
  delete_date           : {type: Date, default: null, select: false},
  last_login_date       : {type: Date, default: null, select: false},
  status                : {type: Number, default: 0, select: false},
  posts_count           : {type: Number, default: 0, select: true},
  following             : {type: Array, default: [], select: false},
  followers             : {type: Array, default: [], select: false},
  following_count       : {type: Number, default: 0, select: true},
  followers_count       : {type: Number, default: 0, select: true},
  trusts                : {type: [trustSchema], default:[], select: false},
  trusts_count          : {type: Number, default: 0, select: true},
  instagram_token       : {type: String, default: null, select: false},
  instagram_min_id      : {type: String, default: null, select: false}
},{ versionKey          : false });

userSchema.statics.canResolve = function(){
  return [
    {following: {identifier: '_id' , model: 'User'}},
    {followers: {identifier: '_id' , model: 'User'}},
    {trusts: {identifier: 'user_id', model: 'User'}}
  ];
};

userSchema.methods.short = function(fields){
  var response = this.shortUser();

  if(fields){
    for(var index in fields){
      response[fields[index]] = this[fields[index]];
    }
  }
  return response;
};

userSchema.methods.createUserSalt = function(){
  return _serial.stringify(this._id+this.create_date.valueOf()+this.email);
};

userSchema.methods.hashPassword = function(){
  if(this.password && this.create_date && this.email){
    var user_salt = this.createUserSalt();
    var old_pass = this.password;
    this.password = _utils.prismEncrypt(this.password, user_salt);
    if(this.password != old_pass && this.password.length > old_pass.length){
      return true;
    }
  }
  return false;
};

userSchema.methods.findByFacebookId = function(fb_id, callback){
  return this.model('User').findOne({ provider: 'facebook',
                                      provider_id: fb_id }, callback);
};

userSchema.methods.findByTwitterId = function(tw_id, callback){
  return this.model('User').findOne({ provider: 'twitter',
                                      provider_id: tw_id }, callback);
};

userSchema.methods.findByGoogleId = function(google_id, callback){
  return this.model('User').findOne({ provider: 'google',
                                      provider_id: google_id }, callback);
};

userSchema.methods.doesTrustExist = function(user_id){
  if(this.trusts_count === 0){
    return false;
  }else{
    for(var i = 0; i < this.trusts.length; i++){
      if(this.trusts[i].user_id.toString() === user_id.toString()){
        return true;
      }
    }
    return false;
  }
};

userSchema.methods.previousTrustCancelled = function(user_id){
  if(this.trusts_count === 0){
    return false;
  }else{
    for(var i = 0; i < this.trusts.length; i++){
      if(this.trusts[i].user_id.toString() === user_id.toString()){
        if(this.trusts[i].status === 'cancelled' || this.trusts[i].status === 'canceled') return true;
      }
    }
  }
  return false;
};

userSchema.methods.fetchTrustIndexByUserId = function(user_id){
  if(this.trusts_count > 0){
    for(var i=0; i <= this.trusts.length; i++){
      if(this.trusts[i].user_id.toString() === user_id.toString()) return i;
    }
  }else{
    return false;
  }
  return false;
};

userSchema.methods.fetchTrustIndex = function(trust_id, cb){
  if(this.trusts_count > 0){
    for(var i=0; i <= this.trusts.length; i++){
      if(this.trusts[i]._id.toString() === trust_id.toString()){
        cb(i);
        return;
      }
    }
  }else{
    cb(null);
  }
};

userSchema.methods.refresh = function(cb){
  this.model('User').findOne({_id: this._id}, function(err, user){
    if(err) throw err;
    cb(user);
  });
};

userSchema.methods.cleanUserJSON = function(){
  var user = this.toObject();
          delete user.password;
          delete user.comments;
          delete user.likes;
          delete user.provider_token;
          delete user.provider_token_secret;
  return user;
};

userSchema.methods.shortUser = function(){
  var short_user = {
        _id: this._id,
        name: this.name,
        first_name: this.first_name,
        last_name: this.last_name,
        profile_photo_url: this.profile_photo_url
      };
  return short_user;
};

userSchema.pre('save', function(next){
  //set create & modify dates
  if(!this.create_date){
    this.create_date = Date.now();
    this.name = this.first_name + ' ' + this.last_name;
    if(this.password){
      if(!this.hashPassword()) next(false);
    }
    next();
  }else{
    this.modify_date = Date.now();
    if(this.name !== this.first_name + ' ' + this.last_name){
      this.name = this.first_name + ' ' + this.last_name;
    }
    next();
  }
});

exports.User = _mongoose.model('User', userSchema);
exports.Trust = _mongoose.model('Trust', trustSchema);
