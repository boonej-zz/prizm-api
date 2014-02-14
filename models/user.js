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
	first_name          : {type: String, required: true},
	last_name           : {type: String, required: true},
	email               : { type: String, 
                          required: true,
                          index: {unique: true} },
  password            : {type: String},
  provider            : String,
  provider_id         : String, 
  provider_token      : String,
  last_provider_auth  : Date,
  gender              : String,
  birthday            : String,
	address	            : String,
	city                : String,
	country             : String,
	region              : String,
	zip_postal          : String,
	picture_name        : String,
	picture_path        : String,
	picture_thumb_path  : String,
	create_date	        : Date,
	modify_date	        : Date,
	delete_date	        : Date,
  last_login_date     : Date,
	status              : Number,
	comments            : [],
	posts               : [],
	likes               : []
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

userSchema.pre('save', function(next){
  //set create & modify dates
  if(!this.create_date){
    this.create_date = Date.now();
    if(this.password){
      if(!this.hashPassword()) next(false);
      // console.log(JSON.stringify(this));
    }
  }
  this.modify_date = Date.now();
  next();
});

exports.User = _mongoose.model('User', userSchema);
