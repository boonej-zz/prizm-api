/***********************************************************
 ****                User Model                         ****
 **********************************************************/

var mongoose  = require('mongoose')
  , uuid      = require('node-uuid');

var userSchema = new mongoose.Schema({
	first_name			: String,
	last_name			: String,
	email				: String,
	address				: String,
	city				: String,
	country				: String
	region				: String,
	zip_postal			: String,
	picture_name		: String,
	picture_path 		: String,
	picture_thumb_path	: String,
	create_date			: Date,
	modify_date			: Date,
	delete_date			: Date,
	status 				: String,
	comments			: [],
	posts				: [],
	likes				: []
});

exports.User = mongoose.model('User', userSchema);