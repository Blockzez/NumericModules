--[[
	Version 2.0.0a2
	This is intended for Roblox ModuleScripts.
	It works on vanilla Lua, but there are far superior implementations you should use instead.
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
local weakkey = { __mode = 'k' };
local proxy = { bi = setmetatable({ }, weakkey), byte = setmetatable({ }, weakkey) };

-- Hash
local weakval = { __mode = 'v', };
local hashes = { [false] = setmetatable({ }, weakval), [true] = setmetatable({ }, weakval) };
local strong_ref = { };

local bigint_mt = { __metatable = "The metatable is locked"; };

--[=[ Quick functions ]=]--
local function div(left, right)
	return math.floor(left / right);
end;

local function divmod(left, right)
	return div(left, right), left % right;
end;

local function intband(left, right)
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

-- Lua 5.3
local typeof = typeof or type;
local function math_type(x)
	if math.type then
		return math.type(x);
	end;
	return 'float';
end;

-- For Lua 5.3 users
local is51 = ('%15d'):format(9007199254740992 + 1) == "9007199254740992";
local function newproxy_or_setmetatable(mt)
	-- It doesn't really matter which one
	-- If you prefer metatables you can replace it with "if mt ~= bigint_mt then" instead (if false doesn't work).
	if is51 and newproxy then
		local pointer = newproxy(true);
		local pointer_mt = getmetatable(pointer);
		for k, v in next, mt do
			pointer_mt[k] = v;
		end;
		return pointer;
	end;
	return setmetatable({ }, mt);
end;

--[=[ Hash ]=]--
local function gethash(bytes, sign)
	assert(type(sign) == "boolean", "sign must be either true or false");
	local p0 = hashes[sign];
	for _, v in ipairs(bytes) do
		if not p0[v] then
			p0[v] = setmetatable({ }, weakval);
			strong_ref[p0[v]] = true;
		end;
		p0 = p0[v];
	end;
	return p0;
end;

--[=[ Byte handler ]=]--
--[==[
	This is getting the byte of the value, in little endian order.
	Byte 54 is the sign, as the minimum safe integer is -9'007'199'254'740'991 and the maximum
	is 9'007'199'254'740'991 on Lua 5.1
	-3 = (0; 1; ...; 0)
	-1 = (0; ...; 0)
	0 = (0; ...; 1)
	1 = (1; ...; 1)
	3 = (0; 1; ...; 1)
	I don't know why I haven't gone this method on version 1
	I might even consider strings or full 64 bit of double (if you're in Roblox Lua) for future versions!
]==]--
local function bytehandler_rawset(self, index, value)
	proxy.byte[self].value[index + 1] = value;
	return self;
end;

local function bytehandler_rawget(self, index)
	return proxy.byte[self].value[index + 1];
end;

local function bytehandler_readonly(self)
	while proxy.byte[self].value[#proxy.byte[self].value] == -1 do
		table.remove(proxy.byte[self].value);
	end;
	proxy.byte[self].readonly = true;
	return self;
end;

local function bytehandler_gethash(self)
	return gethash(proxy.byte[self].value, self.sign);
end;

local function bytehandler_iter(self)
	return function(self, i)
		i = i + 1;
		if i >= #self then
			return nil;
		end;
		return i, self[i];
	end, self, -1;
end;

local function bytehandler_increase(self)
	if proxy.byte[self].readonly then
		error("It is now readonly to ensure immutibility", 2);
	end;
	local p = -1;
	repeat
		p = p + 1;
		self[p] = (self[p] == 0) and 1 or 0;
	until self[p] == 1;
	return self;
end;

local function bytehandler_decrease(self)
	if proxy.byte[self].readonly then
		error("It is now readonly to ensure immutibility", 2);
	end;
	local p = -1;
	local len = #self;
	repeat
		if p >= len then
			error("Stack Underflow", 2);
		end;
		p = p + 1;
		self[p] = (self[p] == 0) and 1 or 0;
	until self[p] == 0;
	return self;
end;

local function bytehandler_index(self, index)
	if index == "rawset" then
		return bytehandler_rawset;
	elseif index == "readonly" then
		return bytehandler_readonly;
	elseif index == "gethash" then
		return bytehandler_gethash;
	elseif index == "iter" then
		return bytehandler_iter;
	elseif index == "sign" then
		return proxy.byte[self].sign;
	elseif index == "copy" then
		return bytehandler_copy;
	elseif index == "increase" or index == "decrease" then
		return index == "increase" and bytehandler_increase or bytehandler_decrease;
	end;
	local resultind, byteind = divmod(index, is51 and 54 or 64);
	local byte = proxy.byte[self].value[resultind + 1] or -1;
	if byteind == (is51 and 53 or 63) then
		return byte < 0 and 0 or 1;
	end;
	if byte < 0 then
		return div((byte + 1), -(2 ^ byteind)) % 2;
	end;
	return div(byte, (2 ^ byteind)) % 2;
end;

local function bytehandler_newindex(self, index, value)
	if proxy.byte[self].readonly then
		error("It is now readonly to ensure immutibility", 2);
	elseif index == "sign" then
		proxy.byte[self].sign = value;
	elseif self[index] ~= value then
		local b = proxy.byte[self].value;
		local resultind, byteind = divmod(index, is51 and 54 or 64);
		if b[resultind + 1] or value ~= 0 then
			if resultind > #b then
				for i = #b + 1, resultind do
					b[i] = -1;
				end;
			end;
			local byte = b[resultind + 1] or -1;
			if byteind == (is51 and 53 or 63) then
				b[resultind + 1] = (-byte) - 1;
			else
				b[resultind + 1] = byte + ((2 ^ byteind) * ((value == 0 and -1 or 1) * (byte < 0 and -1 or 1)));
			end;
		end;
	end;
end;

local function bytehandler_len(self)
	return (#proxy.byte[self].value) * (is51 and 54 or 64);
end;

local bytehandler_mt =
{
	__index = bytehandler_index;
	__newindex = bytehandler_newindex;
	__len = bytehandler_len;
};

function bytehandler_copy(self)
	local value = proxy.byte[self].value;
	
	local pointer = newproxy_or_setmetatable(bytehandler_mt)
	proxy.byte[pointer] = { readonly = false, value = table.move(value, 1, #value, 1, table.create(#value)) };
	return pointer;
end;

local function createbits()
	local pointer = newproxy_or_setmetatable(bytehandler_mt);
	proxy.byte[pointer] = { readonly = false, value = { } };
	
	return pointer;
end;

--[=[ Base bit creation ]=]--
local function generatebits(tbl, sign)
	local ret = createbits();
	for k, v in ipairs(tbl) do
		ret[k - 1] = v;
	end;
	ret.sign = sign;
	return ret;
end;

--[==[ Pseudo BigInteger, this is NOT immutable, and NOT for consumer use ]==]--
local pseudo_mt =
{
	__add = function(self, other)
		if other == 0 then
			return self;
		end;
		assert(self.base == other.base, "base is inconsistent");
		local c = 0;
		for i, v in ipairs(other.value) do
			c, self.value[i] = divmod(c + (self.value[i] or 0) + v, self.base);
		end;
		if c > 0 then
			self.value[#other.value + 1] = 1;
		end;
		return self;
	end;
	__sub = function(self, other)
		if other == 0 then
			return self;
		end;
		assert(self.base == other.base, "base is inconsistent");
		if self < other then
			return nil;
		end;
		local c = 0;
		for i, v in ipairs(other.value) do
			c, self.value[i] = divmod(c + self.value[i] - v, self.base);
		end;
		if c < 0 then
			table.remove(self.value);
		end;
		while self.value[#self.value] == 0 do
			table.remove(self.value);
		end;
		return self;
	end;
	__mul = function(self, other)
		for i = 2, other do
			self = self + self;
		end;
		return self;
	end;
	__compare = function(self, other)
		assert(self.base == other.base, "base is inconsistent");
		if #self.value ~= #other.value then
			return (#self.value < #other.value) and -1 or 1;
		end;
		for i = #self.value, 1, -1 do
			if self.value[i] ~= other.value[i] then
				return (self.value[i] < other.value[i]) and -1 or 1;
			end;
		end;
		return 0;
	end;
	__lt = function(self, other)
		return getmetatable(self).__compare(self, other) < 0;
	end;
	__le = function(self, other)
		return getmetatable(self).__compare(self, other) <= 0;
	end;
	__eq = function(self, other)
		return getmetatable(self).__compare(self, other) == 0;
	end;
};
local function pseudo_bigint(...)
	local value, base;
	if select('#', ...) == 1 then
		base = ...;
		value = { };
	else
		value, base = ...;
	end;
	return setmetatable({ value = value, base = base }, pseudo_mt);
end;

local function copy_pseudo_bigint(value, base)
	return pseudo_bigint(table.move(value.value, 1, #value.value, 1, table.create(#value.value)), base);
end;

local function to_bits(v0, base, sign)
	if base == 2 then
		return generatebits(v0, sign):readonly();
	end;
	local ret = { };
	local v1 = pseudo_bigint({ 1 }, base);
	local v2 = (pseudo_bigint(v0, base) - (sign and 0 or v1));
	-- negative zero
	if not v2 then
		v2 = v0;
		sign = true;
	end;
	local value_access = { pseudo_bigint({ 1 }, base) };
	while v1 <= v2 do
		table.insert(value_access, copy_pseudo_bigint(v1 * 2, base));
	end;
	for i = #value_access, 1, -1 do
		local v3 = v2 - value_access[i];
		ret[i] = v3 and 1 or 0;
	end;
	
	return generatebits(ret, sign):readonly();
end;

local function from_bits(v0, base)
	if base == 2 then
		return v0;
	end;
	if base ~= nil and not tonumber(base) then
		error("invalid argument #2 (number expected, got " .. typeof(base) .. ')', 2);
	elseif base ~= nil and base < 2 and base > 36 then
		error("invalid argument #2 (base out of range)");
	end;
	local ret = pseudo_bigint(base);
	local v1 = pseudo_bigint({ 1 }, base);
	if not v0.sign then
		ret = ret + v1;
	end;
	for i, v in v0:iter() do
		if (v == 1) or (v == true) then
			ret = ret + v1;
		end;
		v1 = v1 * 2;
	end;
	return ret.value;
end;

--[=[ Main ]=]--
local function check_bigint(func, params, err)
	return function(...)
		local argc = select('#', ...);
		if argc < math.abs(params) then
			error(("missing argument #%d (bigint expected)"):format(argc + 1));
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
					error(((err or "invalid argument #{0} (bigint expected, got {1})"):gsub("{0}", i):gsub("{1}", typeof(v))), 2);
				end;
			end;
		end;
		for i = params + 1, argc do
			args[i] = (select(i, ...));
		end;
		return func(unpack(args));
	end;
end;

--[==[ Direct methods ]==]--
local function rawnew(bits)
	bits:readonly();
	local hash = bits:gethash();
	if hash.hash then
		return hash.hash;
	end;
	
	local pointer = newproxy_or_setmetatable(bigint_mt);
	hash.hash = pointer;
	proxy.bi[pointer] = { name = "bigint", bits = bits; };
	
	return pointer;
end;
local values = { };

local function add(self, other)
	if self == values.zero then
		return other;
	elseif other == values.zero then
		return self;
	end;
	
	local data0 = proxy.bi[self].bits;
	local data1 = proxy.bi[other].bits;
	local ret = createbits();
	ret.sign = data0.sign;
	local carry = 0;
	for i = 0, math.max(#data0, #data1) - 1 do
		local diff;
		diff = data0[i] + (data1[i] * ((data0.sign == data1.sign) and 1 or -1)) + carry;
		carry, ret[i] = divmod(diff, 2);
	end;
	if not data1.sign then
		(data0.sign and bytehandler_decrease or bytehandler_increase)(ret);
	end;
	
	if carry == -1 then
		carry = 0;
		local ret1 = createbits();
		ret1.sign = not ret.sign; 
		local len = #ret;
		for i = 0, len do
			carry, ret1[i] = divmod(((i == len and 0 or 1) - ret[i]) + carry, 2);
		end;
		ret = ret1;
	end;

	return rawnew(ret);
end;
local function unm(self)
	if self == values.zero then
		return self;
	end;
	local ret = proxy.bi[self].bits:copy();
	ret.sign = not proxy.bi[self].bits.sign;
	(ret.sign and bytehandler_increase or bytehandler_decrease)(ret);
	return rawnew(ret);
end;
local function mul(self, other)
	if self == values.zero or other == values.zero then
		return values.zero;
	elseif self == values.one then
		return other;
	elseif other == values.one then
		return self;
	elseif self == values.negative_one then
		return unm(other);
	elseif other == values.negative_one then
		return unm(self);
	end;
	
	local positive = (proxy.bi[self].bits.sign == proxy.bi[other].bits.sign);
	local data0 = proxy.bi[self:abs()].bits;
	local data1 = proxy.bi[other:abs()].bits;
	local p, q = #data0, #data1;
	local ret = createbits();
	local tot = 0;
	for ri = 0, p + q do
		for bi = math.max(0, ri - p), math.min(ri, q) do
			local ai = ri - bi;
			tot = tot + (data0[ai] * data1[bi]);
		end;
		tot, ret[ri] = divmod(tot, 2);
	end;
	ret[p + q] = tot % 2;
	ret.sign = positive;
	if not positive then
		ret:decrease();
	end;
	return rawnew(ret);
end;
local function shl(self, ...)
	local other = ...;
	if proxy.bi[other] then
		other = other:todouble();
	elseif select('#', ...) == 0 then
		error("missing argument #2 (number expected)", 2);
	elseif (not tonumber(other)) then
		error("invalid argument #2 (bigint/number expected got " .. typeof(other) .. ')', 2);
	else
		other = math.floor(other);
	end;
	local data = proxy.bi[self].bits;
	if not data.sign then
		data = data:copy():increase();
	end;
	local ret = createbits();
	local contain_one = true;
	for i = 0, #data + other - 1 do
		ret[i] = data[i - other];
		contain_one = contain_one or (ret[i] ~= 0);
	end;
	if (not data.sign) and contain_one then
		ret:decrease();
		ret.sign = false;
	else
		ret.sign = true;
	end;
	return rawnew(ret);
end;
local function shr(self, ...)
	local other = ...;
	if proxy.bi[other] then
		other = other:todouble();
	elseif select('#', ...) == 0 then
		error("missing argument #2 (number expected)", 2);
	elseif (not tonumber(other)) then
		error("invalid argument #2 (bigint/number expected got " .. typeof(other) .. ')', 2);
	else
		other = math.floor(other);
	end;
	local data = proxy.bi[self];
	local ret = createbits();
	for i = other, #data.bits do
		ret[i] = data.bits[i + other];
	end;
	if #ret == 0 and data.sign == - 1 then
		return values.negative_one;
	end;
	return rawnew(ret, data.sign);
end;
local function band(self, other)
	local p, ret = 1, 0;
	while self > values.zero and other > values.zero do
		local r0, r1 = self % 2, other % 2;
		if (r0 + r1) > values.one then
			ret = ret + p;
		end;
		self, other, p = (self - r0) / 2, (other - r1) / 2, p * 2;
	end;
	return ret;
end;
local function bnot(self)
	local p, ret = 1 , 0;
	while self > values.zero do
		local r = self % 2;
		if r < values.one then
			ret = ret + p 
		end
		self, p = (self - r) / 2, p * 2;
	end;
	return ret;
end;
local function bor(self, other)
	local p, ret = 1, 0;
	while self + other > values.zero do
		local r0, r1 = self % 2, other % 2;
		if r0 + r1 > values.zero then 
			ret = ret + p;
		end;
		self, other, p = (self - r0) / 2, (other - r1) / 2, p * 2;
	end;
	return ret;
end;
local function bxor(self, other)
	local p, ret = 1, 0;
	while self > values.zero and other > values.zero do
		local r0, r1 = self % 2, other % 2;
		if r0 ~= r1 then 
			ret = ret + p;
		end;
		self, other, p = (self - r0) / 2, (other - r1) / 2, p * 2;
	end
	if self < other then
		self = other;
	end;
	while self > values.zero do
		local r = self % 2;
		if r > values.zero then
			ret = ret + p;
		end;
		self, p = (self - r) / 2, p * 2
	end
	return ret;
end;
local function divrem(self, other)
	-- NOT modulus, this is C remainder
	-- If you're looking for modulus use the :Modulus() metamethod
	-- https://rob.conery.io/2018/08/21/mod-and-remainder-are-not-the-same/
	-- https://stackoverflow.com/questions/13683563/whats-the-difference-between-mod-and-remainder
	if other == values.zero then
		error("division or remainder by zero", 2);
	elseif self == other then
		return values.one, values.zero;
	elseif self == values.zero then
		return values.zero, values.zero;
	elseif self == values.one then
		return values.zero, values.one;
	elseif self == values.negative_one then
		return values.zero, values.negative_one;
	elseif other == values.one then
		return self, values.zero;
	elseif other == values.negative_one then
		return unm(self), values.zero;
	end;
	
	local sign0 = proxy.bi[self].bits.sign;
	self = self:abs();
	local data0 = proxy.bi[self].bits;
	local sign1 = proxy.bi[other].bits.sign and 1 or - 1;
	other = other * sign1;
	
	if self < other then
		return values.zero, self * (sign0 and 1 or -1);
	end;
	
	local ret, rem = values.zero, values.zero;
	for i = #data0 - 1, 0, -1 do
		rem = shl(rem, 1);
		local b = proxy.bi[rem].bits:copy();
		b[0] = data0[i];
		b.sign = true;
		rem = rawnew(b);
		if rem >= other then
			rem = rem - other;
			b = proxy.bi[ret].bits:copy();
			b[i] = 1;
			b.sign = true;
			ret = rawnew(b);
		end;
	end;
	return ret * ((sign0 and 1 or -1) * sign1), rem * (sign0 and 1 or -1);
end;
local function concat(self, other)
	return tostring(self) .. tostring(other);
end;
local function islessthanbit(left, right)
	-- Positive value override negative
	local sign0, sign1 = math.sign(left), math.sign(right);
	if sign0 ~= sign1 then
		return sign0 < sign1;
	end;
	-- Check the absolute value
	return math.abs(left) < math.abs(right);
end;

local function compare(self, other)
	if proxy.bi[self].bits.sign ~= proxy.bi[other].bits.sign then
		return proxy.bi[self].bits.sign and 1 or -1;
	end;
	local data0 = proxy.byte[proxy.bi[self].bits].value;
	local data1 = proxy.byte[proxy.bi[other].bits].value;
	
	if #data0 ~= #data1 then
		return ((#data0 < #data1) == proxy.bi[self].bits.sign) and -1 or 1;
	end;
	for i = #data0, 1, -1 do
		if data0[i] ~= data1[i] then
			return (islessthanbit(data0[i], data1[i]) == proxy.bi[self].bits.sign) and -1 or 1;
		end;
	end;
	return 0;
end;
local function toint53array(bits)
	local ret0 = { };
	for i = 0, #bits - 1 do
		if i % 53 == 0 then
			table.insert(ret0, bits[i]);
		else
			ret0[#ret0] = ret0[#ret0] + (bits[i] * (2 ^ (i % 53)));
		end;
	end;
	if ret0[#ret0] == 0 then
		table.remove(ret0);
	end;
	return ret0;
end;
local function log(self, other)
	local data = toint53array(proxy.bi[self].bits);
	if proxy.bi[other] then
		other = other:todouble();
	elseif (not tonumber(other)) and other ~= nil then
		error("invalid argument #2 (bigint/number expected got " .. typeof(other) .. ')', 2);
	else
		other = tonumber(other) or 2.71828182845905;
	end;
	if (not proxy.bi[self].bits.sign) or other == 1 then
		return tonumber('nan');
	elseif self == values.one then
		return 0;
	elseif math.abs(other) == tonumber('inf') or other == 0 then
		return tonumber('nan');
	end;
	
	local r0, r1 = 0, 0.5;
	local topbits = 0;
	local r2 = data[#data];
	while r2 > 0 do
		r2 = math.floor(r2 / 2);
		topbits = topbits + 1;
	end;
	local bitlength = (#data - 1) * 53 + topbits;
	local indbit = (2 ^ (topbits - 1));
	
	for i = #data, 1, -1 do
		while indbit ~= 0 do
			if (intband(data[i], indbit) ~= 0) then
				r0 = r0 + r1;
			end;
			r1 = r1 * 0.5;
			indbit = math.floor(indbit / 2);
		end;
		indbit = 2 ^ 53;
	end;
	return (math.log(r0) + (.69314718055994529 * bitlength)) / math.log(other);
end;
local function pow(self, ...)
	local other = (...);
	if proxy.bi[other] then
		other = other:todouble();
	elseif select('#', ...) == 0 then
		error("missing argument #2 (number expected)", 2);
	elseif (not tonumber(other)) then
		error("invalid argument #2 (bigint/number expected got " .. typeof(other) .. ')', 2);
	else
		other = math.floor(other);
	end;
	if other == 0 then
		return values.one;
	elseif other == 1 then
		return self;
	elseif other < 0 then
		return values.zero;
	elseif self == values.one or self == values.zero then
		return self;
	elseif self == values.negative_one then
		return band(other, self) ~= 0 and 1 or -1;
	end;
	local ret = values.one;
	while other ~= 0 do
		if intband(other, 1) ~= 0 then
			ret = ret * self;
		end;
		other = math.floor(other / 2);
		self = self * self;
	end;
	return ret;
end;

--[=[ Class methods ]=]--
local bi = setmetatable({ }, { 
	__newindex = function(self, ind, func)
		rawset(self, ind, check_bigint(func, 1));
	end;
});

-- For scientific notation, currencies and compact numbers tolocalestring is more than enough
local base_char = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
local function concat_base(bits, base)
	local tbl = from_bits(bits, base);
	if base <= 10 then
		return table.concat(tbl, ''):reverse():gsub('^0+', '');
	end;
	local r = '';
	for _, v in ipairs(tbl) do
		r = base_char:sub(v - 1, v - 1) .. r;
	end;
	return r:gsub('^0+', '');
end;
function bi.tostring(self, options)
	local ret = concat_base(proxy.bi[self].bits, (type(options) == "number") and options or 10);
	if ret == '' then
		ret = '0';
	end;
	if type(options) ~= "table" and type(options) ~= "number" and options ~= nil then
		error("invalid argument #2 (string expected, got " .. typeof(options) .. ')', 2);
	elseif options then
		local minimumIntegerDigits = (options.minimumIntegerDigits or 0);
		local maximumIntegerDigits = (options.maximumIntegerDigits or math.huge);
		if minimumIntegerDigits < options.maximumIntegerDigits then
			error("the maximumIntegerDigits is bigger than minimumIntegerDigits");
		end;
		if maximumIntegerDigits and #ret > maximumIntegerDigits then
			ret = ret:sub(-options.maximumIntegerDigits);
		end;
		if minimumIntegerDigits and #ret < minimumIntegerDigits then
			ret = ('0'):rep(minimumIntegerDigits - #ret) .. ret;
		end;
		if #ret > 2 + (options.minimumGroupingDigits or 1) then
			local rem;
			options.groupSize = options.groupSize or { };
			options.groupSize[1] = options.groupSize[1] or 3;
			options.groupSize[2] = options.groupSize[2] or options.groupSize[1] or 3;
			ret, rem = (options.groupSymbol or ' ') .. ret:sub(-options.groupSize[1]), ret:sub(1, -(options.groupSize[1] + 1));
			while #rem > options.groupSize[2] do
				ret, rem = (options.groupSymbol or ' ') .. ret:sub(-options.groupSize[2]), ret:sub(1, -(options.groupSize[2] + 1));
			end;
			ret = rem .. ret;
		end;
		if options.minimumFractionDigits then
			ret = ret .. (options.decimalSymbol or '.') .. ('0'):rep(options.minimumFractionDigits);
		end;
	end;
	return (proxy.bi[self].bits.sign and '' or '-') .. ret;
end;
function bi.bin(self)
	return (('0b' .. concat_base(proxy.bi[self].bits, 2)):gsub('^0b$', '0b0'));
end;
function bi.hex(self)
	return (('0x' .. concat_base(proxy.bi[self].bits, 16)):gsub('^0x$', '0x0'));
end;

function bi.mod(self, other)
	local ret = self % other;
	return (ret < 0) and (ret + other) or ret;
end;
rawset(bi, 'mod', check_bigint(bi.modulus, 2, "attempt to perform arithmetic (mod) on {0}"));

function bi.divmod(self, other)
	local r0, r1 = divrem(self, other);
	return r0 + (((r0 < values.zero) and (r1 == values.zero)) and 1 or 0), (r1 < values.zero) and (r1 + other) or r1;
end;
rawset(bi, 'divmod', check_bigint(bi.divmod, 2, "attempt to perform arithmetic (divmod) on {0}"));

rawset(bi, 'divrem', check_bigint(divrem, 2, "attempt to perform arithmetic (divrem) on {0}"));
rawset(bi, 'compare', check_bigint(compare, 2, "attempt to compare {0}"));

rawset(bi, 'band', check_bigint(band, 2, check_bigint(band, 2, "attempt to perform bitwise operation (band) on {0}")));
rawset(bi, 'bor', check_bigint(band, 2, check_bigint(bor, 2, "attempt to perform bitwise operation (bor) on {0}")));
rawset(bi, 'bxor', check_bigint(band, 2, check_bigint(bxor, 2, "attempt to perform bitwise operation (bxor) on {0}")));

bi.bnot = bnot;

function bi.iseven(self)
	return proxy.bi[self].bits[0] == (proxy.bi[self].bits.sign and 0 or 1);
end;

function bi.sign(self)
	if self == values.zero then
		return 0;
	end;
	return proxy.bi[self].bits.sign and 1 or -1;
end;

function bi.todouble(self)
	local ret = 0;
	for i, v in proxy.bi[self].bits:iter() do
		ret = ret + (v * (2 ^ i));
	end;
	return (ret + (proxy.bi[self].bits.sign and 0 or 1)) * (proxy.bi[self].bits.sign and 1 or -1);
end;

function bi.toint32(self)
	return self:todouble() % (2 ^ 31);
end;

function bi.min(value, ...)
	local argc = select('#', ...);
	local min_val = value;
	for i = 1, argc do
		local v = constructor(select(i, ...));
		if v then
			if v < min_val then
				min_val = v;
			end;
		else
			error((("invalid argument #{0} (bigint expected, got {1})"):gsub("{0}", i + 1):gsub("{1}", typeof(v))), 2);
		end;
	end;
	return min_val;
end;

function bi.max(value, ...)
	local argc = select('#', ...);
	local max_val = value;
	for i = 1, argc do
		local v = constructor(select(i, ...));
		if v then
			if v < max_val then
				max_val = v;
			end;
		else
			error((("invalid argument #{0} (bigint expected, got {1})"):gsub("{0}", i + 1):gsub("{1}", typeof(v))), 2);
		end;
	end;
	return max_val;
end;

bi.pow = pow;
bi.log = log;
function bi.log10(self)
	return log(self, 10);
end;
function bi.log2(self)
	return log(self, 2);
end
function bi.log16(self)
	return log(self, 36);
end;
function bi.log12(self)
	return log(self, 12);
end;
function bi.log8(self)
	return log(self, 8);
end;

function bi.abs(self)
	if proxy.bi[self].bits.sign then
		return self;
	end;
	local ret = proxy.bi[self].bits:copy();
	ret.sign = true;
	ret:increase();
	return rawnew(ret);
end;

getmetatable(bi).__newindex = nil;
bi.copysign = check_bigint(function(self, sign)
	local ret = proxy.bi[self].bits:copy();
	ret.sign = not proxy.bi[sign].bits.sign;
	if ret.sign ~= proxy.bi[self].bits.sign then
		(ret.sign and bytehandler_decrease or bytehandler_increase)(ret);
	end;
	return rawnew(ret);
end, -2);

bi.clamp = check_bigint(function(self, min, max)
	if max < min then
		error("max must be greater than min", 2);
	elseif self < min then
		return min;
	elseif self > max then
		return max;
	end;
	return self;
end, 3);

--[==[ Metamethods ]==]--
bigint_mt.__index = bi;
function bigint_mt.__tostring(self, options)
	return self:tostring(options);
end;
bigint_mt.__concat = concat;

bigint_mt.__abs = bi.abs;
bigint_mt.__log = bi.log;
function bigint_mt.__round(self)
	return self;
end;
function bigint_mt.__repr(self)
	return ("n'%s'"):format(tostring(self));
end;

setmetatable(bigint_mt, { __newindex = function(self, index, func) rawset(self, index, check_bigint(func, 2, " attempt to perform arithmetic (" .. index:sub(3):gsub('mod', 'rem') .. ") on {0}")) end; })
bigint_mt.__add = add;
bigint_mt.__unm = unm;
function bigint_mt.__sub(self, other)
	return add(self, unm(other));
end;
bigint_mt.__mul = mul;
function bigint_mt.__div(self, other)
	return (divrem(self, other));
end;
function bigint_mt.__mod(self, other)
	return (select(2, divrem(self, other)));
end;
bigint_mt.__pow = pow;
-- Lua 5.3
bigint_mt.__idiv = bigint_mt.__div;

-- Just in case for Lua 5.3 users
getmetatable(bigint_mt).__newindex = function(self, index, func) rawset(self, index, check_bigint(func, 2, " attempt to compare {0}")) end;
function bigint_mt.__lt(self, other)
	return compare(self, other) < 0;
end;
function bigint_mt.__le(self, other)
	return compare(self, other) <= 0;
end;
function bigint_mt.__eq(self, other)
	return compare(self, other) == 0;
end;

getmetatable(bigint_mt).__newindex = function(self, index, func) rawset(self, index, check_bigint(func, 2, " attempt to perform bitwise operation (" .. index:sub(3) .. ") on {0}")); end;
bigint_mt.__band = band;
bigint_mt.__bor = bor;
bigint_mt.__bxor = bxor;
bigint_mt.__bnot = bnot;
bigint_mt.__shl = shl;
bigint_mt.__shr = shr;

setmetatable(bigint_mt, nil);

--[=[ Predefined value ]=]
-- Minus One
values.negative_one = createbits();
values.negative_one.sign = false;
values.negative_one = rawnew(values.negative_one);

-- Zero
values.zero = createbits();
values.zero.sign = true;
values.zero = rawnew(values.zero);

-- One
values.one = createbits():rawset(0, -2);
values.one.sign = true;
values.one = rawnew(values.one);

--[==[ Constructor ]==]--
function constructor(...)
	if select('#', ...) == 0 then
		error("missing argument #1", 2);
	elseif proxy.bi[(...)] then
		return (...);
	end;
	local value, base = ...;
	if base ~= nil and not tonumber(base) then
		error("invalid argument #2 (number expected, got " .. typeof(base) .. ')', 2);
	elseif base ~= nil and (base < 2 or base > 36 or (base % 1) ~= 0) then
		error("invalid argument #2 (base out of range)", 2);
	end;
	base = base or 10;
	local bits, sign;
	if value == true then
		return values.one;
	elseif value == false then
		return values.zero;
	elseif type(value) == "number" then
		sign, value = value >= 0, math.abs(value);
		if value < (2 ^ 53) then
			bits = createbits():rawset(0, -(value + (sign and 1 or 0)));
			bits.sign = sign;
		else
			return constructor(math_type(value) == "float" and ('%.0f'):format(value) or tostring(value));
		end;
	else
		if type(value) ~= "string" then
			value = tostring(value);
		end;
		value = value:gsub('[.,]%d+$', ''):upper();
		
		sign, value = value:match('^([%+%-]?)(.*)$');
		if sign == '+' then
			sign = '';
		end;
		
		local int, frac, exp = value:match('(%d*)[.,]?(%d*)E([+]?%d+)');
		if int and base == 10 then
			exp = tonumber(exp);
			if not exp then
				return nil;
			end;
			if int == '' and frac == '' then
				return nil;
			end;
			if exp < -1 then
				return values.zero;
			end;
			frac = frac:gsub('0+$', '');
			value = int .. frac .. ('0'):rep(exp - #frac);
		end;
		
		if not (value and (value:match("^[^  _].*[^  _]$") or value:match('^[^  _]$'))) then
			return nil;
		end;
		sign, value = sign == '', value:gsub('[  _]', ''):gsub('^0+', '');
		
		if value == '' then
			return values.zero;
		end;
		
		local base_ret = { };
		for i = #value, 1, -1 do
			local v = base_char:find(value:sub(i, i));
			if not v then
				return nil;
			end;
			table.insert(base_ret, v - 1);
		end;
		bits = to_bits(base_ret, base, sign);
	end;
	
	return rawnew(bits);
end;

local function isbigint(...)
	if select('#', ...) == 0 then
		error("missing argument #1", 2);
	end;
	return not not proxy.bi[(...)];
end;

return setmetatable(
	{ },
	{
		__index = function(_, ind)
			if ind == "new" then 
				return constructor;
			elseif ind == "isbigint" then 
				return isbigint; 
			end;
			return bi[ind];
		end,
		__metatable = "The metatable is locked",
		__newindex = function()
			error("Attempt to modify a readonly table", 2);
		end;
	}
);
