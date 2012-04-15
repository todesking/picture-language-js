var Scheme={};
Scheme.Util={
argsAsArray:
	function(args) {
		var r=[];
		for(var i=0;i<args.length;i++)
			r.push(args[i]);
		return r;
	}
};

Scheme.Logger={
info:
	function(msg) {console.info(msg);},
error:
	function(msg) {console.error(msg);}
};

Scheme.inspectSExpr=function(expr) {
	if(expr===void(0)) return 'undefined';
	else if(expr===null) return '()';
	else if(expr===false) return 'false'
	else if(expr===true) return 'true'
	else if(expr.isContinuation) return '#<continuation>';
	else if(expr.isLambda) return '#<closure>';
	else if(typeof(expr)=='string') return '"'+expr+'"';
	else if(typeof(expr)=='function') return '#<js function>';
	else return expr.toString();
}
Scheme.parse=function(src) {
	var g=new Gin.Grammar({
	Expr: /(Number | String | Symbol | List ):push/,
	Number: /Float | Integer/,
	Float: /$REAL ::float/,
	Integer: /$INT ::integer/,
	String: /$JS_STRING ::string/,
	Symbol: /(<[-+*\/><=_$!?a-zA-Z.][-+*\/><=_$!?a-zA-Z0-9.]*>) ::symbol/,
	List: /'(':beginList Expr* ')' ::list/
	},'Expr',Gin.SPACE);
	var makeList=function(cars,cdr) {
		var result=cdr;
		for(var i=cars.length-1;i>=0;i--)
			result=new Cons(cars[i],result);
		return result;
	};
	var action={
	stack: [[]],
	integer:
		function(v) {return parseInt(v[0]);},
	float:
		function(v) {return parseFloat(v[0]);},
	symbol:
		function(v) {return new Symbol(''+v[0])},
	string:
		function(v) {return ''+v[0]},
	push:
		function(v) {this.stack.last().push(v)},
	beginList:
		function(v) {this.stack.push([])},
	list:
		function(v) {
			var cars=this.stack.pop();
			return makeList(cars,null);
		}
	};
	var match=g.parse(src,action);
	var result=(match&&match.full)?action.stack[0][0]:void(0);
	if(result===void(0)) Scheme.Logger.error('parse error' + src);
	return result;
}

Scheme.evaluate=function(expr,env) {
	var ops=Scheme.compile(expr);
	return Scheme.run(ops,env)
}
Scheme.run=function(ops,env) {
	var vm=new Scheme.VM();
	vm.run(ops,env);
	return vm.ax;
}
Scheme.compile=function(expr) {
	return Scheme.Compiler.compile(expr,['halt']);
}
Scheme.Compiler={
compile:
	function(expr,next) {
		var c=Scheme.Compiler;
		if(expr && expr.isCons) 
			return c.compile_list(expr,next);
		else if(expr && expr.isSymbol)
			return ['ref',expr.name,next];
		else
			return ['const',expr,next];
	},
compile_list:
	function(expr,next) {
		if(!next) throw 'compile_list: next instruction is unspecified'
		var c=Scheme.Compiler;
		if(expr.car.isSymbol) {
			//compile syntax
			switch(expr.car.name) {
			case 'define':
				if(expr.cdr.car.isCons) { //(define (proc args...) body)
					var transformed=Cons.fromArray(
						[new Symbol('define'),expr.cdr.car.car,Cons.fromArray([new Symbol('lambda'),expr.cdr.car.cdr].concat(expr.cdr.cdr.toArray()))]);
					return c.compile(transformed,next)
				} else { //(define name value)
					return c.compile(expr.nthCar(2),['define',expr.nthCar(1).name,next]);
				}
			case 'if':
				return c.compile(expr.nthCar(1),['test',c.compile(expr.nthCar(2),next),c.compile(expr.nthCar(3),next)]);
			case 'let':
				var body=expr.cdr.cdr
				var defs=expr.cdr.car
				var params=[]
				var args=[]
				var cur=defs
				while(cur) {
					params.push(cur.car.car)
					args.push(cur.car.cdr.car)
					cur=cur.cdr
				}
				var body_expr=Cons.fromArray([new Symbol('lambda'),Cons.fromArray(params)],body)
				return c.compile_apply(Cons.fromArray([body_expr].concat(args)),next)
			case 'begin':
				return c.compile(Cons.fromArray([new Symbol('let'),null],expr.cdr),next)
			case 'quote':
				return ['const',expr.cdr.car,next];
			case 'lambda':
				return c.compile_lambda(expr,next);
			case 'set!':
				return c.compile(expr.nthCar(2),['set',expr.nthCar(1).name,next]);
			default:
				return c.compile_apply(expr,next);
			}
		} else {
			return c.compile_apply(expr,next);
		}
	},
compile_apply:
	function(expr,next) {
		//alert('compile_apply: '+Scheme.inspectSExpr(expr)+' '+Scheme.inspectAssembly(next))
		if(!expr.car) throw 'compile: invalid expression found: apply to null '+Scheme.inspectSExpr(expr)
		var c=Scheme.Compiler;
		var args=expr.cdr?expr.cdr.toArray():[]

		var ref_apply=expr.car.isSymbol
		var const_call=true //引数が全部定数だったら展開できるよね
		var simple_call=false //ref/constのみ まだ最適化がうまくいってないので無効化
		args.each(function(a) {
			if(a && a.isSymbol) { //reference
				const_call=false
			} else if(!a || !a.isCons) { //const
			} else { //other (call proc)
				const_call=false
				simple_call=false
			}
		});

		var apply_ops=null
		if(simple_call) {
			apply_ops=['apply-prepared',next]
		} else if(ref_apply) {
			apply_ops=['apply-ref',expr.car.name]
		} else {
			apply_ops=c.compile(expr.car,['apply']);
		}

		var call_ops=null
		if(args.length==0) {
			call_ops=apply_ops
		} else if(const_call) { 
			call_ops=c.compile(expr.car,['apply-with-constant',args])
		} else {
			call_ops=apply_ops
			args.clone().reverse().each(function(a) {
				if(a && a.isSymbol) { //reference
					call_ops=['param-ref',a.name,call_ops]
					const_call=false
				} else if(!a || !a.isCons) { //const
					call_ops=['param-const',a,call_ops]
				} else {
					call_ops=c.compile(a,['param',call_ops]);
					const_call=false
					simple_call=false
				}
			});
		}
		if(next[0]!='return') {
			if(simple_call && ref_apply) {
				call_ops=['prepare-proc-from-ref-and-push-if-need',expr.car.name,next,call_ops]
				call_ops=c.compile(expr.car,call_ops)
			} else {
				call_ops=['push',next,call_ops];
			}
		}
		return call_ops;
	},
compile_lambda:
	function(expr,next) {
		//todo: next=='apply'だった場合の最適化(let)
		//bodyをコンパイルし、
		var bodylist=[];
		for(var cur=expr.cdr.cdr;cur&&cur.isCons;cur=cur.cdr) 
			bodylist.push(cur.car);
		var body=['return'];
		for(var i=bodylist.length-1;i>=0;i--)
			body=Scheme.Compiler.compile(bodylist[i],body);
		//引数を束縛する命令を先頭にくっつける
		var params=[];
		for(var cur=expr.nthCar(1);cur && cur.isCons;cur=cur.cdr) {
			if(!cur.car.isSymbol) {
				Scheme.Logger.error('compile error: '+cur.car+'is not symbol');
				return void(0);
			}
			params.push(cur.car.name);
		}
		if(params.length==0) { //引数なしのときは高速化の余地あり
			body=['clearargs',body];
			return ['lambda-noparams',body,next]
		} else {
			body=['bindargs',params,body];
			return ['lambda',body,next];
		}
	}
}
Scheme.inspectAssembly=function(asm) {
	return asm.inspect(); //てぬき
}
Scheme.VM=Class.create();
Scheme.VM.prototype={
initialize:
	function() {
		this.ax=null;
		this.frame=null;
		this.params=[];
		this.ip=null;
		this.env=null;
	},
run:
	function(ops,base_env) {
		if(!base_env) {
			Scheme.Logger.error('VM.run : env is null!!!');
			this.ax=void(0);
			return;
		}
		var ise=Scheme.inspectSExpr
		Scheme.Logger.info('evaluating...'+ops.inspect());
		this.env=base_env;
		this.ip=ops;
		operator_table=Scheme.VM.operator_table
		while(this.ip[0]!='halt') {
			var ip=this.ip;
			var op=operator_table[ip[0]]
			if(!op) {
				this.error_halt('unknown operation found: '+ip[0]);
			} else {
				op(this,ip);
			}
		}
		Scheme.Logger.info('VM stopped.result='+Scheme.inspectSExpr(this.ax));
		return this.ax;
	},
error_halt:
	function(message) {
		var i=Scheme.inspectSExpr
		Scheme.Logger.error(message + "\n" +
			['ax: '+i(this.ax),'frame: '+this.frame,'params: '+(this.params?this.params.map(i):i(this.params)),'env: '+this.env].join('\n'));
		this.ax=void(0);
		this.ip=['halt'];
	}
}

Scheme.VM.operator_table= {
	ref: function(self,ip) { //ref var next ;変数参照
		self.ax=self.env.lookup(ip[1]);
		if(self.ax===void(0)) {
			self.error_halt('ref: undefined variable: '+ip[1]);
			return
		} else {
			self.ip=ip[2];
		}
	},
	'const': function(self,ip) { // const expr next ;定数参照
		self.ax=ip[1];
		self.ip=ip[2];
	},
	test: function(self,ip) { //test then else; ax==()かどうかで分岐
		if(self.ax===void(0)) {
			self.error_halt('test op when ax==undef');
		} if(self.ax) {
			self.ip=ip[1];
		} else { //null,false,0. 本家ではどうなってたっけ
			self.ip=ip[2];
		}
	},
	param: function(self,ip) { //param next ;axをパラメータスタックにpush
		self.params.push(self.ax);
		self.ip=ip[1];
	},
	'param-ref': function(self,ip) { //param-ref symbol next
		var val=self.env.lookup(ip[1]);
		if(val===void(0)) {
			self.error_halt('param-ref: undefined variable: '+ip[1]);
			return
		}
		self.params.push(val)
		self.ip=ip[2]
	},
	'param-const': function(self,ip) { //param-const value next
		self.params.push(ip[1])
		self.ip=ip[2]
	},
	'apply-ref': function(self,ip) { //apply symbol ;本当に速くなるんかいな
		//以下、refのインライン展開
		target=self.env.lookup(ip[1]);
		if(target===void(0)) {
			self.error_halt('apply-ref: undefined variable: '+ip[1]);
			return
		}
		//以下、applyのインライン展開
		if(!target) {
			self.error_halt('trying apply to null or undefind')
			return
		} else if(target.isLambda) { // lambda
			self.env=(target.noparams?target.env : new Environment(target.env));
			self.ip=target.body;
		} else if(typeof(target)=='function') { // js function
			try {
				self.ax=target.apply(null,self.params);
			} catch(e) {
				self.error_halt('primitive function throws exception :' + e)
				return
			}
			if(self.ax===void(0)) self.ax=null;
			// self.ip=['return']の展開
			var f=self.frame;
			if(!f) { self.error_halt('return when frame is empty'); return; }
			self.frame=f.parent;
			self.params=f.params.clone();
			self.env=f.env;
			self.ip=f.next;
			return
		} else if(target.isContinuation) { //continuation
			if(self.params.length!=1) {
				self.error_halt('call continuation with wrong number of arguments');
				return
			}
			self.ax=self.params[0];
			self.frame=target.frame;
			self.ip=['return'];
		} else {
			self.error_halt("apply-ref: can't apply to "+Scheme.inspectSExpr(target));
		}
	},
	'apply-with-constant': function(self,ip) {
		self.params=ip[1]
		Scheme.VM.operator_table.apply(self,ip)
	},
	'apply-prepared': function(self,ip) { //apply-prepared next
		var target=self.prepared_proc
		if(typeof(target)=='function') {
			try {
				self.ax=target.apply(null,self.params);
			} catch(e) {
				self.error_halt('primitive function throws exception :' + e)
				return
			}
			if(self.ax===void(0)) self.ax=null;
			self.ip=ip[1]
			self.params=self.params_backup
			self.params_backup=null
			self.prepared_proc=null
			return
		}
		self.ax=self.prepared_proc
		self.prepared_proc=null
		Scheme.VM.operator_table.apply(self,ip);
	},
	apply: function(self,ip) { //apply ;axに入ってるクロージャだか何かを呼ぶ
		var target=self.ax;
		if(!target) {
			self.error_halt('trying apply to null or undefind')
			return
		} else if(target.isLambda) { // lambda
			self.env=(target.noparams?target.env : new Environment(target.env));
			self.ip=target.body;
		} else if(typeof(target)=='function') { // js function
			try {
				self.ax=target.apply(null,self.params);
			} catch(e) {
				self.error_halt('primitive function throws exception :' + e)
				return
			}
			if(self.ax===void(0)) self.ax=null;
			// self.ip=['return']の展開
			var f=self.frame;
			if(!f) { self.error_halt('return when frame is empty'); return; }
			self.frame=f.parent;
			self.params=f.params.clone();
			self.env=f.env;
			self.ip=f.next;
			return
		} else if(target.isContinuation) { //continuation
			if(self.params.length!=1) {
				self.error_halt('call continuation with wrong number of arguments');
				return
			}
			self.ax=self.params[0];
			self.frame=target.frame;
			self.ip=['return'];
		} else {
			self.error_halt("apply: can't apply to "+Scheme.inspectSExpr(self.ax));
		}
	},
	bindargs: function(self,ip) { //bindargs params next ;引数を束縛し、params,axレジスタをクリアする
		if(ip[1].length != self.params.length) {
			self.error_halt('unmatched param length(expected '+ip[1].length+' but '+self.params.length+'. args=['+ip[1]+'] params=['+self.params.map(function(p){return Scheme.inspectSExpr(p)})+'])')
			return
		}
		for(var i=0;i<ip[1].length;i++) {
			self.env.bind(ip[1][i],self.params[i]);
		}
		self.params=[];
		self.ip=ip[2];
		self.ax=null;
	},
	 clearargs: function(self,ip) { //clear-params next ;params,axレジスタをクリアする
		self.params=[]
		self.ax=null
		self.ip=ip[1]
	},
	lambda: function(self,ip) { //lamda body next ;クロージャ作る
		self.ax={
		isLambda: true,
		body: ip[1],
		noparams: false,
		env: self.env
		};
		self.ip=ip[2];
	},
	'lambda-noparams': function(self,ip) { //lambda-noparams body next
		//todo: そもそも呼ばれた側で環境を生成するかどうか判断できる(何でそうなってないんだっけ？)
		self.ax={
		isLambda: true,
		body: ip[1],
		noparams: true,
		env: self.env
		};
		self.ip=ip[2];
	},
	'prepare-proc-from-ref-and-push-if-need': function(self,ip) { //prepare-proc-from-ref-and-push-if-need name return next
		var proc=self.env.lookup(ip[1])
		if(proc===void(0)) {
			self.error_halt('prepare-proc-from-ref-and-push-if-need: undefined variable: '+ip[1]);
			return;
		}
		self.prepared_proc=proc;
		if(typeof(proc)=='function') {
			self.params_backup=self.params
		} else {
			//pushの展開,ipのフォーマットが違う
			self.frame= {
			next: ip[2],
			params:self.params,
			env:self.env,
			parent:self.frame
			}
		}
		self.ax=null;
		self.params=[];
		self.ip=ip[3]
	},
	push: function(self,ip) { //push return next ;現在の状態をスタックにpush
		self.frame= {
		next: ip[1],
		params:self.params,
		env:self.env,
		parent:self.frame
		}
		self.params=[];
		self.ax=null;
		self.ip=ip[2];
	},
	'return': function(self,ip) { //return ;現在のフレームから抜ける。axがretvalとなる
		var f=self.frame;
		if(!f) { self.error_halt('return when frame is empty'); return; }
		self.frame=f.parent;
		self.params=f.params.clone();
		self.env=f.env;
		self.ip=f.next;
	},
	set: function(self,ip) { //set name next ;変数設定
		if(!self.env.defined(ip[1])) {
			self.error_halt('set: name '+ip[1]+' is unbound');
			return;
		}
		self.env.set(ip[1],self.ax);
		self.ip=ip[2];
		self.ax=null;
	},
	'current-continuation': function(self,ip) { //current-continuation next ; save current frame.
		self.ax={
		isContinuation: true,
		frame: self.frame
		}
		self.ip=ip[1];
	},
	define: function(self,ip) { //define var next ;変数定義 ほんとはどこでもできちゃまずいんだけど
		self.env.bind(ip[1],self.ax);
		self.ip=ip[2];
		self.ax=null;
	}
}

Scheme.getDefaultEnvironment=function() {
	if(Scheme.defaultEnvironment) return Scheme.defaultEnvironment;
	Scheme.defaultEnvironment=new Environment(null);
	var de=Scheme.defaultEnvironment;
	de.bind('cons',function(car,cdr) {return new Cons(car,cdr)});
	de.bind('car',function(cons){if(!cons.isCons) throw 'car: '+Scheme.inspectSExpr(cons)+' is not cons'; return cons.car});
	de.bind('cdr',function(cons){if(!cons.isCons) throw 'cdr: '+Scheme.inspectSExpr(cons)+' is not cons'; return cons.cdr});
	de.bind('+',function() {
		return $A(arguments).inject(0,function(a,v){return a+Number(v);});
	});
	de.bind('-',function() {
		if(arguments.length==0) throw '-: no arguments'
		if(arguments.length==1) return -Number(arguments[0])
		return $A(arguments).inject(Number(arguments[0])+Number(arguments[0]),function(a,v){return a-Number(v);});
	});
	de.bind('*',function() {
		return $A(arguments).inject(1,function(a,v){return a*Number(v);});
	});
	de.bind('/',function() {
		if(arguments.length!=2) throw '/ operator gives only two arguments'
		return Number(arguments[0])/Number(arguments[1])
	})
	de.bind('list',function() {
		var result=null;
		for(var i=arguments.length-1;i>=0;i--)
			result=new Cons(arguments[i],result);
		return result;
	});
	de.bind('append-string',function() { return $A(arguments).join('') });
	de.bind('>',function(a,b) { return Number(a)>Number(b); });
	de.bind('<',function(a,b) { return Number(a)<Number(b); });
	de.bind('=',function(a,b) { return Number(a)==Number(b); });
	de.bind('<=',function(a,b) {return Number(a)<=Number(b); })
	de.bind('evalu',function(expr,env) { return Scheme.evaluate(expr,env?env:de) });
	de.bind('call-with-current-continuation',{
	isLambda:true,
	body: ['bindargs',['callback'],['current-continuation',['param',['ref','callback',['apply']]]]],
	env: new Environment(de)
	});
	de.bind('js-new',function(klass,args){ throw 'not implemented yet' })
	de.bind('js-eval',function(js_src){return eval(js_src)});
	function js_get_prop(target,propname) {
		propnames=propname.split('.')
		var cur=target;
		while(cur && propnames.length) cur=cur[propnames.shift()]
		if(propnames.length) throw 'prop not found: '+propname;
		return cur;
	}
	function js_set_prop(target,propname,value) {
		propnames=propname.split('.')
		var cur=target;
		while(cur && 1<propnames.length) cur=cur[propnames.shift()]
		if(1<propnames.length) throw 'prop not found: '+propname
		cur[propnames[0]]=value;
	}
	de.bind('js-get-prop',js_get_prop);
	de.bind('js-set-prop',js_set_prop);
	//calleeがsymbol/stringのときはcallerのプロパティを探す
	de.bind('js-apply',function(caller,callee,args) {
		if(!callee) {
			throw 'js-apply: callee is null or undefined'
		} else if(typeof(callee)!='function') {
			if(typeof(callee)=='string') {
				callee=js_get_prop(caller,callee)
			} else if(callee instanceof Symbol) { //symbol
				callee=js_get_prop(caller,callee.name)
			} else {
				throw 'js-apply: callee is unknown type'+Scheme.inspectSExpr(callee)
			}
		}
		if(!args || typeof(args)!='object' || !args.isCons) //リストが渡されなかったら1引数として処理
			args=[args]
		return callee.apply(caller,args.toArray())
	})
	de.bind('js-typeof',function(x){return typeof(x)})
	de.bind('js-to-list',function(array) {
		return Cons.fromArray(array);
	});
	de.bind('call/cc',de.lookup('call-with-current-continuation'));
	de.bind('inspect-closure',function(clo){return Scheme.inspectAssembly(clo.body)});
	de.bind('null?',function(x){return x===null});
	return de;
}

var Environment=Class.create();
Environment.prototype={
initialize:
	function(parent) {
		this.members={};
		this.parent=parent;
	},
bind:
	function(sym,val) {
		this.members[sym]=val;
	},
lookup:
	function(sym) {
		var cur=this
		while(cur) {
			var val=cur.members[sym];
			if(val!==void(0)) return val;
			cur=cur.parent
		}
		try {
			return eval(sym);
		} catch(e) {
			return void(0);
		}
	},
set:
	function(sym,val) {
		if(this.definedCurrentLevel(sym)) this.members[sym]=val;
		else if(this.parent) this.parent.set(sym,val);
		else return void(0);
	},
defined:
	function(sym) {
		return this.definedCurrentLevel(sym) || !!(this.parent&&this.parent.defined(sym))
	},
definedCurrentLevel:
	function(sym) {
		return !!(this.members[sym]!==void(0));
	},
toString:
	function() {
		names=[]
		for(var name in this.members) {
			names.push(name)
		}
		return '{'+names+'}'+this.parent
	},
dummy: void(0)
};

var Cons=Class.create();
Cons.fromArray= function(array,tail) {
	var cur=tail?tail:null
	for(var i=array.length-1;0<=i;i--)
		cur=new Cons(array[i],cur)
	return cur
};
Cons.prototype={
initialize:
	function(car,cdr) {
		this.car=car;
		this.cdr=cdr;
		this.isCons=true;
	},
nthCar:
	function(n) {
		if(n==0) return this.car;
		else return (this.cdr && this.cdr.isCons)?this.cdr.nthCar(n-1):void(0);
	},
toString:
	function() {
		var cur=this;
		var result='('
		while(cur && cur.isCons) {
			result+=Scheme.inspectSExpr(cur.car)+' '
			cur=cur.cdr;
		}
		if(cur!==null) result+='. '+Scheme.inspectSExpr(cur);
		result+=')';
		return result;
	},
toArray:
	function() {
		result=[this.car]
		if(!this.cdr) return result
		if(!this.cdr.isCons) return void(0) //Improper list
		return result.concat(this.cdr.toArray())
	},
dummy: void(0)
};

Scheme.symTable={}
var Symbol=function(name) {
	if(Scheme.symTable[name]) return Scheme.symTable[name];
	this.name=name;
	this.isSymbol=true;
	Scheme.symTable[name]=this;
	this.toString=function(){return name}
	this.inspect=function(){return name}
}
