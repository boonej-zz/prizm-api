var mongoose          = require('mongoose'),
    utils             = require(process.env.PRISM_HOME + 'utils'),
    User              = mongoose.model('User'),
    ObjectId          = mongoose.Schema.Types.ObjectId;

var organizationSchema = new mongoose.Schema({
  code                : {type: String, default: null, required: true, 
                          index: true},
  theme               : {type: ObjectId, ref: 'Theme', required: false},
  name                : {type: String, default: null, required: true},
  create_date         : {type: Date, default: null, required: false},
  modify_date         : {type: Date, default: null, required: false},
  members             : {type: Array, default: []}

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
      'members'];
  return select;
};

organizationSchema.methods.format = function(type, add_fields){
  format = {
    _id:          this._id,
    code:         this.code,
    create_date:  this.create_date,
    modify_date:  this.modify_date,
    members:      this.members,
    name:         this.name,
    theme:        this.theme
  };
  return format;
}

organizationSchema.statics.canResolve = function(){
  return [
    {members: {identifier: '_id', model: 'User'}},
    {theme: {identifier: 'code', model: 'Theme'}}
  ];
}

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
