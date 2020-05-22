--[[
	Version 1.0.2 - 22 May 2020
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
]]--
local bi = { };
local bi_proxy_data = { };

--[=[ Private functions ]=]--
local function band(left, right)
	local p, ret = 1, 0;
	while left > 0 and right > 0 do
		local r0, r1 = left % 2, right % 2;
		if (r0 + r1) > 1 then
			ret = ret + p;
		end;
		left, right, p = (left - r0) / 2, (right - r1) / 2, p * 2;
	end;
	return ret;
end;

local function divmod(self, other)
	return math.floor(self / other), self % other;
end;

local function remove_zero(self)
	for i = #self, 1, -1 do
		if self[i] ~= 0 then
			break;
		end;
		table.remove(self, i);
	end;
	return self;
end;

local function to_base2(self, base)
	remove_zero(self);
	if base == 2 then
		return self;
	end;

	local r0 = { { 1 } };
	local r1 = { 1 };
	repeat
		local temp_r = { };
		for i1 = 1, #r1 do
			temp_r[i1 + 1], temp_r[i1] = divmod((temp_r[i1] or 0) + ((r1[i1] or 0) * 2), base);
		end;
		local is_bigger = false;
		for i1 = math.max(#temp_r, #self), 1, -1 do
			if (temp_r[i1] or 0) ~= (self[i1] or 0) then
				is_bigger = (temp_r[i1] or 0) > (self[i1] or 0);
				break;
			end;
		end;
		
		if not is_bigger then
			table.insert(r0, temp_r);
			r1 = temp_r;
		end;
	until is_bigger;
	
	local r2 = { };
	for i0 = #r0, 1, -1 do
		local is_bigger = true;
		for i1 = math.max(#self, #r0[i0]), 1, -1 do
			if (self[i1] or 0) ~= (r0[i0][i1] or 0) then
				is_bigger = (self[i1] or 0) > (r0[i0][i1] or 0);
				break;
			end;
		end;
		
		r2[i0] = is_bigger and 1 or 0;
		
		if is_bigger then
			local temp_r = { };
			for i1 = 1, math.max(#self, #r0[i0]) do
				temp_r[i1 + 1], temp_r[i1] = divmod((temp_r[i1] or 0) + (self[i1] or 0) - (r0[i0][i1] or 0), base);
			end;
			self = temp_r;
		end;
	end;
	return r2;
end;

local function from_base2(self, base)
	remove_zero(self);
	if base == 2 then
		return self;
	end;
	local r0 = { };
	local r1 = { 1 };
	
	for i0 = 1, #self do
		if self[i0] == 1 then
			do
				local temp_r = { };
				for i1 = 1, math.max(#r0, #r1) do
					temp_r[i1 + 1], temp_r[i1] = divmod((temp_r[i1] or 0) + (r0[i1] or 0) + (r1[i1] or 0), base);
				end;
				r0 = temp_r;
			end;
		end;
		do
			local temp_r = { };
			for i1 = 1, #r1 do
				temp_r[i1 + 1], temp_r[i1] = divmod((temp_r[i1] or 0) + ((r1[i1] or 0) * 2), base);
			end;
			r1 = temp_r;
		end;
	end;
	
	return remove_zero(r0);
end;

local function getdata(self, other)
	local ret = { };
	for i0 = 1, #bi_proxy_data[self].bits do
		for i1 = 0, 52 do
			ret[(((i0 - 1) * 53) + i1) + 1] = math.floor(bi_proxy_data[self].bits[i0] / (2 ^ i1)) % 2;
		end;
	end;
	remove_zero(ret);
	return { bits = ret; sign = bi_proxy_data[self].sign };
end;

local function setdata(self)
	local ret = { };
	for i0 = 1, math.floor(#self / 53) + 1 do
		local r = 0;
		for i1 = 0, 52 do
			r = r + ((2 ^ i1) * (self[(((i0 - 1) * 53) + i1) + 1] or 0));
		end;
		ret[i0] = r;
	end;
	remove_zero(ret);
	return ret;
end;

--[=[ Operators and functions ]=]--
local function compare(self, other)
	local data0 = bi_proxy_data[self];
	local data1 = bi_proxy_data[other];
	if data0.sign ~= data1.sign then
		return (data0.sign > data1.sign) and 1 or -1;
	end;
	remove_zero(data0.bits);
	remove_zero(data1.bits);
	if #data0.bits ~= #data1.bits then
		return (#data0.bits > #data1.bits) and 1 or -1;
	end;
	for i = math.max(#data0.bits, #data1.bits), 1, -1 do
		if (data0.bits[i] or 0) ~= (data1.bits[i] or 0) then
			return ((data0.bits[i] or 0) > (data1.bits[i] or 0)) and 1 or -1;
		end;
	end;
	return 0;
end;
local function le(self, other)
	return compare(self, other) <= 0;
end;
local function lt(self, other)
	return compare(self, other) < 0;
end;
local function eq(self, other)
	return compare(self, other) == 0;
end;

local function add(self, other)
	if self == zero then
		return other;
	elseif other == zero then
		return self;
	end;

	local data0 = getdata(self);
	local data1 = getdata(other);
	local ret = { };
	local ret_sign = 1;
	for i = 1, math.max(#data0.bits, #data1.bits) do
		ret[i + 1], ret[i] = divmod(((data0.bits[i] or 0) * data0.sign) + ((data1.bits[i] or 0) * data1.sign) + (ret[i] or 0), 2);
	end;
	if ret[#ret] == -1 then
		local ret1 = { };
		ret[#ret] = 0;
		for i = 1, #ret do
			ret1[i + 1], ret1[i] = divmod(((i == #ret and 1 or 0) - ret[i]) + (ret1[i] or 0), 2);
		end;
		ret = ret1;
		ret_sign = ret_sign * -1;
	end
	
	return rawnew(ret, ret_sign);
end;

local function unm(self)
	local data = getdata(self);
	return rawnew(data.bits, data.sign * -1);
end;

local function sub(self, other)
	return add(self, unm(other));
end;

local function mul(self, other)
	if self == zero or other == zero then
		return zero;
	elseif self == one then
		return other;
	elseif other == one then
		return self;
	end;
	
	local data0 = getdata(self);
	local data1 = getdata(other);
	local p, q = #data0.bits, #data1.bits;
	local ret = { };
	local tot = 0;
	for ri = 1, p + q do
		for bi = math.max(1, ri - p + 1), math.min(ri, q) do
			local ai = ri - bi + 1;
			tot = tot + (data0.bits[ai] * data1.bits[bi]);
		end;
		tot, ret[ri] = divmod(tot, 2);
	end;
	ret[p + q + 1] = tot % 2;
	return rawnew(ret, data0.sign * data1.sign);
end;

local function shl(self, other)
	local data = getdata(self);
	local ret = { };
	for i = 1, #data.bits + other do
		ret[i] = data.bits[i - other] or 0;
	end;
	return rawnew(ret, data.sign);
end;

local function shr(self, other)
	local data = getdata(self);
	local ret = { };
	for i = other, #data.bits do
		ret[i] = data.bits[i + other] or 0;
	end;
	remove_zero(ret);
	if #ret == 0 and data.sign == - 1 then
		new(-1);
	end;
	return rawnew(ret, data.sign);
end;

local function divrem(self, other)
	if other == one then
		return self, zero;
	elseif self == zero then
		return zero, zero;
	elseif other == zero then
		error("division by zero", 2);
	end;
	
	local data0 = getdata(self);
	local sign1 = bi_proxy_data[other].sign;
	other = other * sign1;
	
	if (self * data0.sign) < other then
		return zero, other;
	end;
	local ret, rem = zero, zero;
	for i = #data0.bits, 1, -1 do
		rem = shl(rem, 1);
		local b = getdata(rem).bits;
		b[1] = data0.bits[i];
		rem = rawnew(b, 1);
		if rem >= other then
			rem = rem - other;
			b = getdata(ret).bits;
			b[i] = 1;
			for i1 = 1, i do
				b[i1] = b[i1] or 0;
			end;
			ret = rawnew(b, 1);
		end;
	end;
	return ret * (data0.sign * sign1), rem * (data0.sign);
end;

local function div(self, other)
	return (divrem(self, other));
end;

local function rem(self, other)
	return select(2, divrem(self, other));
end;

local function check(func, msg)
	msg = msg or '';
	return function(self, other)
		local err = false;
		if bi_proxy_data[self] then
		elseif type(self) ~= "number" then
			err = true;
		elseif bi_proxy_data[other] then
		elseif type(other) ~= "number" then
			err = true;
		end;
		if err then
			error(msg:gsub('{0}', typeof(self)):gsub('{1}', typeof(other)), 2);
		end;
		return func(new(self), new(other));
	end;
end;

local base_char = "0123456789abcdefghijklmnopqrstuvwxyz";
local subscript_char =
{
	['0'] = '?',
	['1'] = '¹',
	['2'] = '²',
	['3'] = '³',
	['4'] = '?',
	['5'] = '?',
	['6'] = '?',
	['7'] = '?',
	['8'] = '?',
	['9'] = '?',
};
local compact_table = { 'M', 'G', 'T', 'P', 'E', 'Z', 'Y' };
local function BigInteger_tostring(self, options)
	options = options or { };
	if (options.base or 10) < 2 or (options.base or 10) > 36 then
		error("bad argument #2 (base out of range)", 2)
	end;
	if (options.minimumSignificantDigits or 1) < 1 then
		error("bad argument #2 (minimumSignificantDigits value is out of range)", 2)
	end;
	local data = getdata(self);
	local r0 = from_base2(data.bits, options.base or 10);
	local r1 = '';
	for _, r2 in next, r0 do
		r1 = base_char:sub(r2 + 1, r2 + 1) .. r1;
	end;
	r1 = r1:gsub('^0+', '');
	r0 = ('0'):rep(math.max((options.minimumSignificantDigits or (((options.base == 2) or (options.base == 16)) and options.useGrouping) 
		and (math.ceil(#r0 / ((options.base == 2) and 4 or 2)) * ((options.base == 2) and 4 or 2)) or 1) - #r1, 0)) .. r1;
		
	if (options.base == 10) or (options.base == nil) then
		if options.notation == "scientific" or options.notation == "engineering" then
			return r0:sub(1, 1) .. (((options.decimalComma == false) and '.' or ',') .. r0:sub(2)):gsub('[,.]?0*$', '')
				.. 'E' .. (#r0 - 1)
		elseif options.notation == "standardScientific" then
			return r0:sub(1, 1) .. (((options.decimalComma == false) and '.' or ',') .. r0:sub(2)):gsub('[,.]?0*$', '')
				.. ' × 10' .. tostring(#r0 - 1):gsub('%d', function(v) return subscript_char[v] or '' end);
		elseif options.useGrouping or ((options.notation == "compact") and #r0 < 7) then
			if #r0 > (2 + math.max(options.minimumGroupingDigits or 1, (options.notation == "compact") and 2 or 1)) then
				r0 = r0:sub(1, 1) .. r0:sub(2):reverse():gsub("(%d%d%d)", '%1 '):reverse();
			end;
		elseif options.notation == "compact" then
			local length, size = divmod(#r0 - 4, 3);
			if length > #compact_table then
				length = #compact_table;
				size = #r0 - (#compact_table * 3) - 4;
			end;
			local suffix = compact_table[length];
			r0 = (function(self)
				if #self > math.max(2 + (options.minimumGroupingDigits or 2), 4) then
					return self:sub(1, 1) .. self:sub(2):reverse():gsub("(%d%d%d)", '%1 '):reverse();
				end;
				return #self == 1 and (self .. ((options.decimalComma == false) and '.' or ',') .. r0:sub(2, 2)) or self;
			end)(r0:sub(1, size + 1)) .. ' ' .. suffix;
		end;
	elseif ((options.base == 2) or (options.base == 16)) and options.useGrouping then
		r0 = r0:sub(1, 1) .. r0:sub(2):reverse():gsub(options.base == 16 and '(%d%d)' or '(%d%d%d%d)', '%1 '):reverse();
	end;
	r0 = (data.sign == -1 and '-' or '') .. r0;
	if options.base == 10 or options.base == nil then
		if options.style == "currency" then
			return r0 .. ' ' .. (options.currency or '¤');
		elseif options.style == "percent" then
			return r0:sub(1, 1) .. (r0:sub(2) .. '00'):reverse():gsub("(%d%d%d)", "%1 "):reverse() .. ' %';
		end;
	end;
	return r0;
end;

local function pow(self, other)
	if not bi_proxy_data[self] then
		error("bad argument #2 (BigInteger or number expected got " .. typeof(other) .. ')', 2);
	end;
	if bi_proxy_data[other] then
		other = tonumber(tostring(other));
	elseif type(other) ~= "number" then
		error("bad argument #2 (BigInteger or number expected got " .. typeof(other) .. ')', 2);
	else
		other = math.floor(other);
	end;
	if other == 0 then
		return one;
	elseif other == 1 then
		return self;
	elseif other < 0 then
		return zero;
	elseif self == one or self == zero then
		return self;
	elseif self == minus_one then
		return band(other, self) ~= 0 and 1 or -1;
	end;
	local ret = one;
	while other ~= 0 do
		if band(other, 1) ~= 0 then
			ret = ret * self;
		end;
		other = math.floor(other / 2);
		self = self * self;
	end;
	return ret;
end;

local function index(self, index, value)
	return error("attempt to index BigInteger with '" .. index .. "'");
end;

local function concat(self, other)
	return tostring(self) .. tostring(other);
end;

function rawnew(bits, sign)
	local proxy = newproxy(true);
	bits = setdata(bits);
	bi_proxy_data[proxy] = { bits = bits; sign = (#bits == 0 and 0) or sign; };
	
	local proxy_mt = getmetatable(proxy);
	proxy_mt.__le = le;
	proxy_mt.__lt = lt;
	proxy_mt.__eq = eq;
	proxy_mt.__unm = unm;
	proxy_mt.__add = check(add, "attempt to perform arithmetic (add) on {0} and {1}");
	proxy_mt.__sub = check(sub, "attempt to perform arithmetic (sub) on {0} and {1}");
	proxy_mt.__mul = check(mul, "attempt to perform arithmetic (mul) on {0} and {1}");
	proxy_mt.__div = check(div, "attempt to perform arithmetic (div) on {0} and {1}");
	proxy_mt.__mod = check(rem, "attempt to perform arithmetic (rem) on {0} and {1}");
	proxy_mt.__pow = check(pow, "attempt to perform arithmetic (pow) on {0} and {1}");
	proxy_mt.__concat = concat;
	proxy_mt.__tostring = BigInteger_tostring;
	proxy_mt.__index = index;
	proxy_mt.__newindex = index;
	proxy_mt.__metatable = "The metatable is locked";
	return proxy;
end;

function new(v, base)
	if bi_proxy_data[v] then
		return v;
	end;
	if base == nil then base = 10; end;
	if not tonumber(base) then
		error("bad argument #2 (number expected, got " .. typeof(base) .. ')', 2);
	end;
	base = math.floor(tonumber(base));
	if base < 2 or base > 36 then
		error("bad argument #2 (base out of range)", 2);
	end;
	if type(v) == "number" then
		v = ('%.0f'):format(v);
	end;
	v = tostring(v):lower();
	local int, frac, exp = v:match('(%d*)[.,]?(%d*)e[+]?(%d+)');
	if int and base == 10 then
		exp = tonumber(exp);
		if not exp then
			return nil;
		end;
		if int == '' and frac == '' then
			return nil;
		end;
		frac = frac:gsub('0+$', '');
		if #frac > exp then
			return nil;
		elseif #frac < exp then
			frac = frac .. ('0'):rep(exp - #frac);
		end
		v = int .. frac;
	end;
	if v:sub(1, 1):match('[ ?_]') or v:sub(-1):match('[ ?_]') 
		or v:match('[ ?_][ ?_]') or v:match('[^- ?_' .. base_char .. ']') then
		return nil;
	end;
	v = v:gsub('[ ?_]', '')
	local ret_sign = 1;
	if v:sub(1, 1) == '-' then
		ret_sign = -1;
		v = v:sub(2);
	end;
	local ret = { };
	for i = 1, #v do
		local r = (base_char:find(v:sub(i, i)));
		if (r or 37) > base then
			return nil;
		end;
		table.insert(ret, 1, r - 1);
	end;
	ret = to_base2(ret, base);
	if #ret == 0 then
		ret_sign = 0;
	end;
	return rawnew(ret, ret_sign);
end;

one = new(1);
zero = new(0);
minus_one = new(-1);

local function log(self, other)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	local data = bi_proxy_data[self];
	if bi_proxy_data[other] then
		other = tonumber(tostring(other));
	elseif type(other) ~= "number" and other ~= nil then
		error("bad argument #2 (number expected got " .. typeof(other) .. ')', 2);
	else
		other = other or 2.71828182845905;
	end;
	if data.sign < 0 or other == 1 then
		return tonumber('nan');
	elseif self == one then
		return 0;
	elseif other == tonumber('inf') or other == 0 then
		return tonumber('nan');
	end;
	
	local r0, r1 = 0, .5;
	local topbits = 0;
	local r2 = data.bits[#data.bits];
	while r2 > 0 do
		r2 = math.floor(r2 / 2);
		topbits = topbits + 1;
	end;
	local bitlength = (#data.bits - 1) * 53 + topbits;
	local indbit = (2 ^ (topbits - 1));
	
	for i = #data.bits, 1, -1 do
		while indbit ~= 0 do
			if (band(data.bits[i], indbit) ~= 0) then
				r0 = r0 + r1;
			end;
			r1 = r1 * 0.5;
			indbit = math.floor(indbit / 2);
		end;
		indbit = 2147483648;
	end;
	return (math.log(r0) + (.69314718055994529 * bitlength)) / math.log(other);
end;

--[=[ BigInteger Functions ]=]--
bi.Log = log;
bi.Pow = pow;
bi.DivRem = check(divrem, "attempt to perform divrem on {0} and {1}");
bi.ToString = function(self, options)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	return BigInteger_tostring(self, options);
end;
function bi.ToLocaleString(self, locale, options)
	local msg = "ToLocaleString isn't implemented for this version of the module";
	warn(msg, 2);
	return '';
end;
bi.Shl = check(shl, "attempt to perform bitwise operation (shl) on {0} and {1}");
bi.Shr = check(shr, "attempt to perform bitwise operation (shr) on {0} and {1}");
function bi.TrueDiv(self, other)
	local q, r = divrem(self, other);
	return bi.ToNumber(q) + (bi.ToNumber(r) / bi.ToNumber(other));
end;
function bi.Sign(self)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	return bi_proxy_data[self].sign;
end;
function bi.Abs(self)
	return self * bi.Sign(self);
end;
function bi.Log10(self)
	return log(self, 10);
end;
function bi.Log2(self)
	return log(self, 2);
end;
function bi.Max(...)
	local args = { ... };
	local ret;
	for i, value in next, args do
		if (not bi_proxy_data[value]) and type(value) ~= "number" then
			error("bad argument #" ..  i .. " (BigInteger expected, got " .. typeof(value) .. ')', 2);
		end;
		if not ret then
			ret = value;
		elseif value > ret then
			ret = value;
		end;
	end;
	return ret;
end;
function bi.Min(...)
	local args = { ... };
	local ret;
	for i, value in next, args do
		if (not bi_proxy_data[value]) and type(value) ~= "number" then
			error("bad argument #" ..  i .. " (BigInteger expected, got " .. typeof(value) .. ')', 2);
		end;
		if not ret then
			ret = value;
		elseif value < ret then
			ret = value;
		end;
	end;
	return ret;
end;
function bi.ToNumber(self)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	return tonumber(tostring(self));
end;
function bi.IsBigInteger(self)
	if bi_proxy_data[self] then
		return true;
	end;
	return false;
end;
function bi.Clamp(self, min, max)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	if not bi_proxy_data[min] then
		error("bad argument #2 (BigInteger expected got " .. typeof(min) .. ')', 2);
	end;
	if not bi_proxy_data[max] then
		error("bad argument #3 (BigInteger expected got " .. typeof(max) .. ')', 2);
	end;
	if min > max then
		error("max must be greater or equal than min", 2)
	end
	if self > max then
		return max;
	end;
	if self < min then
		return min;
	end;
	return min;
end;
function bi.GetTableData(self)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	local ret = { };
	for k, v in ipairs(bi_proxy_data[self].bits) do
		ret[k] = v;
	end;
	return { bits = ret, sign = bi_proxy_data[self].sign };
end;
function bi.Compare(self, other)
	if not bi_proxy_data[self] then
		error("bad argument #1 (BigInteger expected got " .. typeof(self) .. ')', 2);
	end;
	if not bi_proxy_data[other] then
		error("bad argument #2 (BigInteger expected got " .. typeof(other) .. ')', 2);
	end;
	return compare(self, other);
end;

bi.Zero = zero;
bi.One = one;
bi.MinusOne = minus_one;
bi.new = new;

return setmetatable(
	bi,
	{
		__newindex = function()
			error("Attempt to modify a readonly table");
		end;
		__metatable = "The metatable is locked";
	}
);
