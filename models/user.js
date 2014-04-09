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

trustSchema.statics.selectFields = function(type){
  var select = ['user_id', 'status','is_owner','create_date','modify_date'];
  if(type === 'basic')
    select.push('delete_date');
  return select;
};

trustSchema.statics.canResolve = function(){
  return [ {user_id: {identifier: '_id', model: 'User'}} ];
};

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
  name                  : {type: String, default: ''},
  first_name            : {type: String, required: true},
  last_name             : {type: String, required: true},
  email                 : {type: String, required: true, index: {unique: true}},
  info                  : {type: String, default: null},
  website               : {type: String, default: null},
  ethnicity             : {type: String, default: null},
  religion              : {type: String, default: null},
  affiliations          : {type: Array, default:[]},
  password              : {type: String, default: null},
  provider              : {type: String, default: null},
  provider_id           : {type: String, default: null},
  provider_token        : {type: String, default: null},
  provider_token_secret : {type: String, default: null},
  last_provider_auth    : {type: Date, default: null},
  gender                : {type: String, default: null},
  birthday              : {type: String, default: null},
  address               : {type: String, default: null},
  city                  : {type: String, default: null},
  country               : {type: String, default: null},
  state                 : {type: String, default: null},
  zip_postal            : {type: String, default: null},
  cover_photo_url       : {type: String, default: ''},
  profile_photo_url     : {type: String, default: ''},
  create_date           : {type: Date, default: null},
  modify_date           : {type: Date, default: null},
  delete_date           : {type: Date, default: null},
  last_login_date       : {type: Date, default: null},
  status                : {type: Number, default: 0},
  posts_count           : {type: Number, default: 0},
  following             : {type: Array, default: []},
  followers             : {type: Array, default: []},
  following_count       : {type: Number, default: 0},
  followers_count       : {type: Number, default: 0},
  trusts                : {type: [trustSchema], default:[]},
  trusts_count          : {type: Number, default: 0},
  instagram_token       : {type: String, default: null},
  instagram_min_id      : {type: String, default: null}
},{ versionKey          : false });

userSchema.statics.canResolve = function(){
  return [
    {following: {identifier: '_id' , model: 'User'}},
    {followers: {identifier: '_id' , model: 'User'}},
    {trusts: {identifier: 'user_id', model: 'User'}}
  ];
};

userSchema.statics.selectFields = function(type){
  if(type === 'short'){
    return ['_id','name','first_name','last_name','profile_photo_url'];
  }else if(type === 'basic'){
    return ['_id','name','first_name','last_name','profile_photo_url',
            'cover_photo_url','email','info','website','city','state',
            'create_date','posts_count','following_count','followers_count',
            'trusts_count'];
  }else{
    return ['_id','name','first_name','last_name','profile_photo_url',
            'cover_photo_url','email','info','website','city','state',
            'create_date','posts_count','following_count','followers_count',
            'trusts_count','provider','provider_id','provider_token',
            'provider_token_secret','gender','birthday','address','country',
            'modify_date','delete_date','status','password'];
  }
};

userSchema.methods.format = function(type, add_fields, callback){
  var format;
  if(!type) type = 'basic';

  if(type === 'short'){
    format = {
      _id:    this._id,
      name:   this.name,
      first_name: this.first_name,
      last_name: this.last_name,
      profile_photo_url: this.profile_photo_url
    };
  }

  if(type === 'basic' || type === 'internal'){
    format = {
      _id:                this._id,
      name:               this.name,
      first_name:         this.first_name,
      last_name:          this.last_name,
      email:              this.email,
      info:               this.info,
      website:            this.website,
      city:               this.city,
      state:              this.state,
      cover_photo_url:    this.cover_photo_url,
      profile_photo_url:  this.profile_photo_url,
      create_date:        this.create_date,
      posts_count:        this.posts_count,
      following_count:    this.following_count,
      followers_count:    this.followers_count,
      trusts_count:       this.trusts_count
    };
  }

  if(type === 'internal'){
    format.password               = this.password;
    format.provider               = this.provider;
    format.provider_id            = this.provider_id;
    format.provider_token         = this.provider_token;
    format.provider_token_secret  = this.provider_token_secret;
    format.gender                 = this.gender;
    format.birthday               = this.birthday;
    format.address                = this.address;
    format.country                = this.country;
    format.modify_date            = this.modify_date;
    format.delete_date            = this.status;
  }

  if(add_fields){
    if(typeof add_fields === 'string') format[add_fields] = this[add_fields];
    if(Array.isArray(add_fields) && add_fields.length > 0){
      for(var i=0; i < add_fields.length; i++){
        format[add_fields[i]] = this[add_fields[i]];
      }
    }
  }
  return format;
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
