/**
 * User Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _serial     = require('serializer'),
    _crypt      = require('crypto'),
    _prism_home = process.env.PRISM_HOME,
    _utils      = require(_prism_home + 'utils');

var userSchema = new _mongoose.Schema({
  name                  : {type: String, default: ''},
	first_name            : {type: String, required: true},
	last_name             : {type: String, required: true},
	email                 : { type: String,
                            required: true,
                            index: {unique: true}
                        },
  info                  : {type: String, default: null},
  website               : {type: String, default: null},
  ethnicity             : {type: String, default: null},
  religion              : {type: String, default: null},
  affiliations          : [],
  password              : {type: String},
  provider              : {type: String},
  provider_id           : {type: String},
  provider_token        : {type: String},
  provider_token_secret : {type: String},
  last_provider_auth    : Date,
  gender                : {type: String, default: null},
  birthday              : {type: String, default: null},
  address               : String,
  city                  : String,
  country               : String,
  state                 : String,
  zip_postal            : String,
  cover_photo_url       : {type: String, default: ''},
  profile_photo_url     : {type: String, default: ''},
  picture_thumb_path    : {type: String, default: ''},
  create_date           : Date,
  modify_date           : Date,
  delete_date           : {type: Date, default: null},
  last_login_date       : {type: Date, default: null},
  status                : {type: Number, default: 0},
  posts_count           : {type: Number, default: 0},
  following             : [],
  followers             : [],
  following_count       : {type: Number, default: 0},
  followers_count       : {type: Number, default: 0}
},
{
  versionKey          : false
}
);

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
    next();
  }
});

exports.User = _mongoose.model('User', userSchema);
