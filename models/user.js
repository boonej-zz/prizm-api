/**
 * User Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose')
  , _serial     = require('serializer')
  , _crypt      = require('crypto')
  , _utils      = require(process.env.PRISM_HOME + 'utils');

var userSchema = new _mongoose.Schema({
	first_name            : {type: String, required: true},
	last_name             : {type: String, required: true},
	email                 : { type: String, 
                            required: true,
                            index: {unique: true} 
                        },
  password              : {type: String},
  provider              : {type: String},
  provider_id           : {type: String},
  provider_token        : {type: String},
  provider_token_secret : {type: String},
  last_provider_auth    : Date,
  gender                : String,
  birthday              : String,
	address	              : String,
	city                  : String,
	country               : String,
	state                 : String,
	zip_postal            : String,
	cover_photo_url       : {type: String, default: ''},
	profile_photo_url     : {type: String, default: ''},
	picture_thumb_path    : String,
	create_date	          : Date,
	modify_date	          : Date,
	delete_date	          : Date,
  last_login_date       : Date,
	status                : Number,
	comments              : [],
	posts                 : [],
	likes                 : []
},
{
  versionKey          : false
}
);

userSchema.methods.createUserSalt = function(){
  return _serial.stringify(this._id+this.create_date.valueOf()+this.email); 
}

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
}

userSchema.methods.findByFacebookId = function(fb_id, callback){
  return this.model('User').findOne({ provider: 'facebook', 
                                      provider_id: fb_id }, callback);
}

userSchema.methods.findByTwitterId = function(tw_id, callback){
  return this.model('User').findOne({ provider: 'twitter',
                                      provider_id: tw_id }, callback);
}

userSchema.methods.findByGoogleId = function(google_id, callback){
  return this.model('User').findOne({ provider: 'google', 
                                      provider_id: google_id }, callback);
}

userSchema.methods.confirmUniqueSocialUser = function(callback){
  return this.model('User').findOne({ provider_id: this.provider_id,
                                      provider: this.provider }, callback);
}

userSchema.pre('save', function(next){
  //set create & modify dates
  if(!this.create_date){
    this.create_date = Date.now();
    
    if(this.provider_id){

      this.confirmUniqueSocialUser(function(err, res){
        if(res) next(false);
      });
    }
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
