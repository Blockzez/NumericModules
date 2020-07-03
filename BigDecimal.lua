--[=[
	Version 1.0.0a3
	This is intended for Roblox ModuleScripts
	BSD 2-Clause Licence
	Copyright ©, 2020 - Blockzez (devforum.roblox.com/u/Blockzez and github.com/Blockzez)
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
	
	1. Redistributions of source code must retain the above copyright notice, this
	   list of conditions and the following disclaimer.
	
	2. Redistributions in binary form must reproduce the above copyright notice,
	   this list of conditions and the following disclaimer in the documentation
	   and/or other materials provided with the distribution.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
]=]--
-- The BigInt module is required
local bigint = require(script.Parent:WaitForChild("BigInteger"));

-- Lua 5.3
local function math_type(x)
	if math.type then
		return math.type(x);
	end;
	return 'float';
end;

local function scale_val(val, exp)
	val = ('0'):rep(-exp) .. val .. ('0'):rep(exp);
	local unscaled = (val:gsub("[.,]", ''));
	local len = #val;
	local dpos = (val:find("[.,]") or (len + 1)) + exp;
	return unscaled:sub(1, dpos - 1) .. '.' .. unscaled:sub(dpos);
end;

local function exp(val)
	local negt, intg, frac = val:gsub('^0+', ''):match('^(-?)([%d%a()]*)[.]?(%d*)$');
	if intg:match('%D') then
		return val;
	end;
	if #intg == 0 then
		local fsize = #frac:gsub('[^0]*$', '');
		return negt .. (frac:sub(fsize + 1, fsize + 1) == '' and '0' or frac:sub(fsize + 1, fsize + 1)) .. ('.' .. frac:sub(fsize + 2)):gsub('[.]0*$', '') .. ('E-' .. fsize + 1):gsub('-0+$', '0');
	end;
	return negt .. (intg:sub(1, 1) == '' and '0' or intg:sub(1, 1)) .. ('.' .. intg:sub(2) .. frac):gsub('[.]0*$', '') .. 'E' .. (#intg - 1);
end;

local proxy = setmetatable({ }, { __mode = "k" });
local bd = { };
local current_context =
{
	Precision = 27,
	MidpointRounding = 0,
	DivisionByZeroError = false,
	InvalidValueReturnsNaN = true,
};

local function checkcontext(context, canbenil)
	if context == nil then
		return bd.GetContext();
	end;
	if type(context) ~= "table" then
		error("The context argument are expected to be table, not " .. typeof(context), 2);
	end;
	if (type(context.Precision) ~= "number" or (context.Precision < 0) or (context.Precision % 1 ~= 0)) 
		and ((not canbenil) or context.Precision ~= nil) then
		error("Precision property is invalid", 2);
	elseif ((not bd.MidpointRounding[context.Rounding]) and (type(context.Rounding) ~= "number" 
		or (context.Rounding > 4 or context.Rounding < 0 or (context.Rounding % 1) ~= 0)))
		and ((not canbenil) or context.Rounding ~= nil) then
		error("MidpointRounding property is invalid", 2);
	elseif type(context.DivisionByZeroError) ~= "boolean" and ((not canbenil) or context.DivisionByZeroError ~= nil) then
		error("DivisionByZeroError property is invalid", 2);
	elseif type(context.InvalidValueReturnsNaN) ~= "boolean" and ((not canbenil) or context.InvalidValueReturnsNaN ~= nil) then
		error("InvalidValueReturnsNaN property is invalid", 2);
	end;
	return context;
end;

local function override_if_not_nil(value, value_to_override)
	if value == nil then
		return value_to_override;
	end;
	return value;
end;

function bd.SetContext(context)
	if context == nil then
		error("The context argument cannot be empty or nil", 2);
	end;
	checkcontext(context, true);
	current_context =
	{
		Precision = context.Precision or current_context.Precision,
		MidpointRounding = bd.MidpointRounding[context.MidpointRounding] or context.MidpointRounding or current_context.MidpointRounding,
		DivisionByZeroError = override_if_not_nil(context.DivisionByZeroError, current_context.DivisionByZeroError);
		InvalidValueReturnsNaN = override_if_not_nil(context.InvalidValueReturnsNaN, current_context.InvalidValueReturnsNaN);
	};
end;

function bd.GetContext()
	return
	{
		Precision = current_context.Precision,
		MidpointRounding = current_context.MidpointRounding,
		DivisionByZeroError = current_context.DivisionByZeroError;
		InvalidValueReturnsNaN = current_context.InvalidValueReturnsNaN;
	};
end;

bd.MidpointRounding = setmetatable(
	{ }, 
	{
		__index =
		{
			ToEven = 0,
			AwayFromZero = 1,
			ToZero = 2,
			ToNegativeInfinity = 3,
			ToPositiveInfinity = 4,
		};
		__newindex = function()
			error("Attempt to modify read-only table");
		end;
		__metatable = "The metatable is locked";
	}
);

--[=[ Check ]=]--
local function check_bigdecimal(func, params, err)
	return function(...)
		local argc = select('#', ...);
		if argc < math.abs(params) then
			error(("missing argument #%d (BigDecimal expected)"):format(argc + 1));
		end;
		local args = { };
		for i = 1, math.abs(params) do
			local v = constructor(select(i, ...));
			if v then
				args[i] = v;
			else
				if params == 2 then
					local arg0, arg1 = ...;
					arg0, arg1 = typeof(arg0), typeof(arg1);
					error((err:gsub("{0}", (arg0 == arg1) and arg0 or (arg0 .. ' and ' .. arg1))), 2);
				else
					error(((err or "invalid argument #{0} (BigDecimal expected, got {1})"):gsub("{0}", i):gsub("{1}", typeof(v))), 2);
				end;
			end;
		end;
		for i = params + 1, argc do
			args[i] = (select(i, ...));
		end;
		return func(unpack(args));
	end;
end;
local zero_bigint = bigint.new(0);
local one_bigint = bigint.new(1);
local ten_bigint = bigint.new(10);
local function get_value_scale(self, other)
	if other then
		local s0, s1 = proxy[self].scale, proxy[other].scale;
		local v0, v1 = proxy[self].value, proxy[other].value;
		if type(s1) ~= "number" then
			error("This method doesn't support NaN or Infinite values", 3);
		end;
		if s0 < s1 then
			return v0 * (ten_bigint ^ (s1 - s0)), v1, s1, s1 - s0;
		elseif s1 < s0 then
			return v0, v1 * (ten_bigint ^ (s0 - s1)), s0, s0 - s1;
		end;
		return v0, v1, s0, 0;
	end;
	return proxy[self].value, proxy[self].scale;
end;

--[=[ Direct methods ]=]--
local function add(self, other)
	if self:IsNaN() or other:IsNaN() then
		return constructor('NaN');
	elseif self:IsInfinity() and other:IsInfinity() then
		if self ~= other then
			return constructor('NaN');
		end
		return constructor(self);
	elseif self:IsInfinity() then
		return constructor(self);
	elseif other:IsInfinity() then
		return constructor(other);
	end;
	local r0, r1, s = get_value_scale(self, other);
	return constructor(r0 + r1, s);
end;
local function unm(self)
	if self:IsNaN() then
		return constructor('NaN');
	elseif self:IsInfinity() then
		return constructor(self:IsPositiveInfinity() and '-Infinity' or 'Infinity');
	end;
	return constructor(-proxy[self].value, proxy[self].scale);
end;
local values = { };
local function mul(self, other)
	if self:IsNaN() or other:IsNaN() then
		return constructor('NaN');
	elseif self:IsInfinity() then
		local sign = other:Sign();
		if sign == 0 then
			return constructor('NaN');
		end;
		return constructor((self:Sign() == sign) and 'Infinity' or '-Infinity');
	end;
	return constructor(proxy[self].value * proxy[other].value, proxy[self].scale + proxy[other].scale);
end;
local function divrem(self, other)
	local r0, r1, s = get_value_scale(self, other);
	local ret, rem = r0:divrem(r1);
	return ret, rem / (ten_bigint ^ s);
end;
local function idiv(self, other)
	local r0, r1 = get_value_scale(self, other);
	return r0 / r1;
end;
local function rem(self, other)
	local r0, r1, s = get_value_scale(self, other);
	return constructor(r0 % r1, s);
end;
local function div(self, other, context)
	context = (type(context == "number") and context) 
		or bd.GetContext(context);
	if self:IsNaN() or other:IsNaN() then
		return constructor('NaN');
	elseif other == values.zero then
		if (type(context) == "number" and bd.GetContext()) or (type(context) == "table" and context.DivisionByZeroError) then
			error("Division by zero", 2);
		end;
		if self == values.zero then
			return constructor('NaN');
		end;
		return constructor(self:Sign() and 'Infinity' or '-Infinity');
	elseif self:IsInfinity() then
		return constructor((self:Sign() == other:Sign()) and '-Infinity' or 'Infinity');
	end;
	local s0, s1 = proxy[self].scale, proxy[other].scale;
	local v0, v1 = proxy[self].value, proxy[other].value;
	local precision = type(context) == "number" or context.Precision;
	if s0 < precision then
		v0 = v0 * (ten_bigint ^ ((precision + 1) - s0));
	elseif s0 > precision then
		v0 = v0 / (ten_bigint ^ (s0 - (precision + 1)));
	end;
	if s1 < (precision + 1) then
		local delta = ((precision + 1) - s1) - ((precision + 1) - s0);
		if delta < 0 then
			v1 = v1 / (ten_bigint ^ -delta);
		else
			v1 = v1 * (ten_bigint ^ delta);
		end;
	elseif s1 > precision then
		v1 = v1 / (ten_bigint ^ ((s1 - (precision + 1)) + ((precision + 1) - s0)));
	end;
	return constructor(v0 / v1, precision + 1):Quantize(precision);
end;
local function compare(self, other)
	if self:IsNaN() or other:IsNaN() then
		-- C# Double.Compare and Java's Compare consider NaN bigger than all values (including Positive Infinity)
		return 1;
	end;
	if self:IsPositiveInfinity() then
		return other:IsPositiveInfinity() and 0 or 1;
	elseif other:IsPositiveInfinity() then
		return -1;
	elseif self:IsNegativeInfinity() then
		return other:IsNegativeInfinity() and 0 or -1;
	elseif other:IsNegativeInfinity() then
		return 1;
	end;
	local r0, r1 = get_value_scale(self, other);
	return bigint.compare(r0, r1);
end;
local function quantize(self, places, midpoint_rounding)
	if (not tonumber(places)) and places ~= nil then
		error("invalid argument #2 (number expected, got " .. typeof(places) .. ')', 2);
	end;
	places = tonumber(places);
	if places and (places % 1 ~= 0) then
		error("invalid argument #2 (places out of range)", 2);
	end;
	midpoint_rounding = (type(midpoint_rounding ~= "table") and (bd.MidpointRounding[midpoint_rounding] or midpoint_rounding)) 
		or bd.GetContext(midpoint_rounding).MidpointRounding;
	if self:IsNaN() then
		return constructor('NaN');
	elseif self:IsInfinity() then
		return nil;
	elseif self == values.zero then
		return constructor(0);
	end;
	local r, increase = nil, 0;
	local v, s = get_value_scale(self);
	local delta = (places or 0) - s;
	if delta >= 0 then
		return self;
	end;
	local sign = v:sign();
	v = v:abs();
	local ten_delta = ten_bigint ^ -delta;
	local ret, rem = v:divrem(ten_delta);
	s = s + delta;
	if r ~= zero_bigint and midpoint_rounding ~= 3 then
		if midpoint_rounding == 4 then
			increase = 1;
		else
			local midpoint = ten_delta / 2;
			if rem > midpoint then
				increase = 1;
			elseif rem == midpoint then
				if midpoint_rounding == 1 then
					increase = 1;
				elseif (midpoint_rounding ~= 2) and (ret % 2 == one_bigint) then
					increase = 1;
				end;
			end;
		end;
	end;
	if not places then
		return (ret + increase) * sign;
	end;
	return constructor((ret + increase) * sign, s);
end;
local function tostr(self)
	if self:IsNaN() then
		return 'NaN';
	elseif self:IsPositiveInfinity() then
		return 'Infinity';
	elseif self:IsNegativeInfinity() then
		return '-Infinity';
	end;
	local sign;
	local value, scale = get_value_scale(self);
	sign, value = tostring(value):match('(-?)(%d*)');
	local val_delta = #value - scale;
	local r = (val_delta < 0 and ('.' .. ('0'):rep(-val_delta)) or (value:sub(1, val_delta) .. '.')) .. (value:sub(math.max(val_delta + 1, 0)):gsub('0+$', ''));
	
	r = sign .. r:gsub('^[.]', '0.'):gsub('[.]$', '');
	return r;
end;
local function todouble(self)
	if self:IsNaN() then
		return tonumber('nan');
	elseif self:IsInfinity() then
		return math.huge * (self:Sign());
	end;
	return proxy[self].value:todouble() / (10 ^ proxy[self].scale);
end;

--[=[ Class methods ]=]--
setmetatable(bd, { 
	__newindex = function(self, ind, func)
		rawset(self, ind, check_bigdecimal(func, 1));
	end;
});

function bd.IsNaN(self)
	return proxy[self].scale == 'NaN';
end;

function bd.IsInfinity(self)
	return proxy[self].scale == 'Infinity';
end;

function bd.IsPositiveInfinity(self)
	return proxy[self].scale == 'Infinity' and proxy[self].value;
end;

function bd.IsNegativeInfinity(self)
	return proxy[self].scale == 'Infinity' and (not proxy[self].value);
end;

function bd.Sign(self)
	if self:IsNaN() then
		return 0;
	elseif self:IsInfinity() then
		return self:IsPositiveInfinity() and 1 or -1;
	end;
	return proxy[self].value:sign();
end;

rawset(bd, 'CopySign', check_bigdecimal(function(self, sign)
	if self:IsNaN() then
		return constructor('NaN');
	elseif sign:IsNaN() then
		sign = values.zero;
	end;
	return constructor(proxy[self].value:copysign(sign:Sign()), proxy[self].scale);
end, -2));

bd.Quantize = quantize;
bd.Round = quantize;
function bd.Floor(self)
	return quantize(self, 0, bd.MidpointRounding.ToNegativeInfinity);
end;
function bd.Ceiling(self)
	return quantize(self, 0, bd.MidpointRounding.ToPositiveInfinity);
end;
bd.Compare = compare;

function bd.Abs(self)
	if self:IsNaN() then
		return 0;
	elseif self:IsInfinity() then
		constructor('Infinity');
	end
	return constructor(proxy[self].value:abs(), proxy[self].scale);
end;

bd.Div = div;

function bd.ToExpString(self)
	return exp(tostring(self));
end;

--[=[ Metamethods ]=]--
local bigdecimal_mt = { __metatable = "The metatable is locked" };
bigdecimal_mt.__index = bd;
function bigdecimal_mt.__newindex(self, index, value)
	if type(index) ~= "string" and type(index) ~= "number" then
		error("invalid argument #2 (string expected, got " .. typeof(index) .. ')', 2);
	end;
	error(index .. " cannot be assigned to", 2);
end;
bigdecimal_mt.__tostring = tostr;
function bigdecimal_mt.__concat(left, right)
	return tostring(left) .. tostring(right);
end;

bigdecimal_mt.__add = check_bigdecimal(add, 2, "attempt to perform arithmetic (add) on {0}");
bigdecimal_mt.__unm = unm;
bigdecimal_mt.__sub = check_bigdecimal(function(left, right)
	return add(left, unm(right));
end, 2, "attempt to perform arithmetic (sub) on {0}");
bigdecimal_mt.__mul = check_bigdecimal(mul, 2, "attempt to perform arithmetic (mul) on {0}");
bigdecimal_mt.__div = check_bigdecimal(function(left, right) return div(left, right); end, 2, "attempt to perform arithmetic (div) on {0}");
bigdecimal_mt.__mod = check_bigdecimal(function(left, right) return rem(left, right); end, 2, "attempt to perform arithmetic (rem) on {0}");
bigdecimal_mt.__pow = function()
	error("The power function isn't supported for BigDecimal", 2);
end;

bigdecimal_mt.__lt = check_bigdecimal(function(left, right)
	return compare(left, right) < 0;
end, 2, "attempt to compare {0}");
bigdecimal_mt.__le = check_bigdecimal(function(left, right)
	return compare(left, right) <= 0;
end, 2, "attempt to compare {0}");
bigdecimal_mt.__eq = check_bigdecimal(function(left, right)
	return compare(left, right) == 0;
end, 2, "attempt to compare {0}");

-- 5.3
bigdecimal_mt.__idiv = check_bigdecimal(idiv, 2, "attempt to perform arithmetic (idiv) on {0}");

--[=[ Constructor ]=]--
function constructor(...)
	local value, scale = ...;
	if select('#', ...) == 0 then
		error("missing argument #1", 2);
	elseif proxy[value] then
		scale = proxy[value].scale;
		value = proxy[value].value;
	else
		if scale ~= nil and not tonumber(scale) then
			error("invalid argument #2 (number expected, got " .. typeof(scale) .. ')', 2);
		elseif scale ~= nil and (scale % 1 ~= 0) then
			error("invalid argument #2 (scale out of range)", 2);
		end;
		if scale or (type(value) ~= "string" and type(value) ~= "number") then
			value = bigint.new(value);
			scale = scale and (scale < 0 and (#tostring(value) + scale) or scale) or 0;
		else
			if type(value) == "number" then
				-- Not completey accurate sadly as lua doesn't allow values over %.99f :(
				-- TODO Implement full double parser.
				value = math_type(value) == "float" and ("%.99f"):format(value) or tostring(value);
			else
				value = tostring(value);
			end;
			local negt, post = value:lower():match("([%+%-]?)(.*)");
			if negt == '+' then
				negt = '';
			end;
			if post == "infinity" or post == "inf" then
				scale = "Infinity";
				value = negt == '';
			else
				if post then
					local val, exp = post:match('(%d*[.,]?%d*)e([-+]?%d+)');
					if val then
						exp = tonumber(exp);
						if not exp then
							return constructor('nil');
						end;
						if val == '' then
							return constructor('nil');
						end;
						post = scale_val((val:gsub(',', '.')), exp);
					end;
					post = (post:gsub("(%d?)[  _](%d?)", '%1%2'));
					post = post:match("(%d*[.,]?%d*)");
				end;
				if post and post ~= '' then
					value = bigint.new(negt .. (post:gsub('[.,]', '')));
					scale = #post - ((post:find('[.,]')) or #post);
				elseif current_context.InvalidValueReturnsNaN 
					or post == "nan" then
					scale = 'NaN';
					value = nil;
				else
					return nil;
				end;
			end;
		end;
	end;
	
	-- Lua 5.1
	if newproxy then
		local pointer = newproxy(true);
		proxy[pointer] = { value = value; scale = scale; };
		
		local pointer_mt = getmetatable(pointer);
		for k, v in next, bigdecimal_mt do
			pointer_mt[k] = v;
		end;
		return pointer;
	end;
	-- Lua 5.2/5.3/5.4
	local mt = setmetatable({ }, bigdecimal_mt);
	proxy[mt] = { value = value, scale = scale };
	return mt;
end;

values.zero = constructor(0);
 
return setmetatable(
	{ },
	{
		__index = function(_, ind) 
			if ind == "new" then 
				return constructor; 
			elseif ind == "NaN" then 
				-- If rawequal(value0, value1) is true, the __eq metamethod will not run (applies to all Lua 5.1, 5.3 and 5.4)
				-- Don't know if I can bypass this
				return constructor('NaN');
			elseif ind == "PositiveInfinity" then
				return constructor('+Inf');
			elseif ind == "NegativeInfinity" then
				return constructor('-Infinity'); 
			elseif not ind:match("to.*locale.*string") then 
				return bd[ind]; 
			end; 
			return nil;
		end,
		__newindex = function() error("Attempt to modify a readonly table", 2); end, 
		__metatable = "The metatable is locked"
	}
);
