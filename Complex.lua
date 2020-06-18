--[[
	Version 1.0.0
	This is intended for Roblox ModuleScripts.
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
local proxy = { };
local i = { };
local methods = setmetatable(
	{ },
	{
		__newindex = function(self, index, func)
			rawset(self, index, function(...)
				if select('#', ...) == 0 then
					error("missing argument #1 (Complex expected)", 2);
				elseif not proxy[(...)] then
					error("invalid argument #1 (Complex expected, got " .. typeof((...)) .. ')', 2);
				end;
				return func(...);
			end);
		end;
	}
);
local i_mt = { __metatable = "The metatable is locked" };

--[=[ Metamethods ]=]--
function i_mt.__add(self, other)
	if type(self) == "number" then
		return i.new(self + proxy[other].Real, proxy[other].Imaginary);
	elseif type(other) == "number" then
		return i.new(proxy[self].Real + other, proxy[self].Imaginary);
	elseif proxy[self] and proxy[other] then
		return i.new(proxy[self].Real + proxy[other].Real, proxy[self].Imaginary + proxy[other].Imaginary);
	end;
	error("attempt to perform arithmetic (add) on " .. (typeof(self) .. " and " .. typeof(other)):gsub(" and " .. typeof(self), ''), 2);
end;
function i_mt.__sub(self, other)
	if type(self) == "number" then
		return i.new(self - proxy[other].Real, proxy[other].Imaginary);
	elseif type(other) == "number" then
		return i.new(proxy[self].Real - other, proxy[self].Imaginary);
	elseif proxy[self] and proxy[other] then
		return i.new(proxy[self].Real - proxy[other].Real, proxy[self].Imaginary - proxy[other].Imaginary);
	end;
	error("attempt to perform arithmetic (sub) on " .. (typeof(self) .. " and " .. typeof(other)):gsub(" and " .. typeof(self), ''), 2);
end;
function i_mt.__mul(self, other)
	if type(self) == "number" then
		return i.new(self * proxy[other].Real, self * proxy[other].Imaginary);
	elseif type(other) == "number" then
		return i.new(proxy[self].Real * other, proxy[self].Imaginary * other);
	elseif proxy[self] and proxy[other] then
		local a, b, c, d = proxy[self].Real, proxy[self].Imaginary, proxy[other].Real, proxy[other].Imaginary;
		return i.new((a * c) - (b * d), (a * d) + (b * c));
	end;
	error("attempt to perform arithmetic (mul) on " .. (typeof(self) .. " and " .. typeof(other)):gsub(" and " .. typeof(self), ''), 2);
end;
function i_mt.__div(self, other)
	if type(self) == "number" then
		local a, c, d = self, proxy[other].Real, proxy[other].Imaginary;
		return i.new((a * c) / ((c ^ 2) + (d ^ 2)), (a * d) / ((c ^ 2) + (d ^ 2)));
	elseif type(other) == "number" then
		return i.new((proxy[self].Real * other) / (other ^ 2), (proxy[self].Imaginary * other) / (other ^ 2));
	elseif proxy[self] and proxy[other] then
		local a, b, c, d = proxy[self].Real, proxy[self].Imaginary, proxy[other].Real, proxy[other].Imaginary;
		return i.new(((a * c) + (b * d)) / ((c ^ 2) + (d ^ 2)), ((b * c) - (a * d)) / ((c ^ 2) + (d ^ 2)));
	end;
	error("attempt to perform arithmetic (mul) on " .. (typeof(self) .. " and " .. typeof(other)):gsub(" and " .. typeof(self), ''), 2);
end;
local E = 2.7182818284590451;
function i_mt.__pow(self, other)
	if type(other) == "number" or (proxy[self] and proxy[other]) then
		local a, b, c, d = proxy[self].Real, proxy[self].Imaginary, proxy[other] and proxy[other].Real or other, proxy[other] and proxy[other].Imaginary or 0;
		if (a == 0 and b == 0) or (c == 0 and d == 0) then
			return i.new(0, 0);
		end;
		local rho = self:Abs();
		local theta = math.atan2(b, a);
		local newrho = (c * theta) + (d * math.log(rho));
		local t = (rho ^ c) * (E ^ (-d * theta));
		return i.new(t * math.cos(newrho), t * math.sin(newrho));
	end;
	error("attempt to perform arithmetic (pow) on " .. (typeof(self) .. " and " .. typeof(other)):gsub(" and " .. typeof(self), ''), 2);
end;
function i_mt.__unm(self)
	return self * -1;
end;
function i_mt.__index(self, index)
	if proxy[self][index] then
		return proxy[self][index];
	elseif methods[index] then
		return methods[index];
	end;
	return nil;
end;
function i_mt.__newindex(self, index, value)
	if type(index) ~= "string" and type(index) ~= "number" then
		error("invalid argument #2 (string expected, got " .. typeof(index) .. ')', 2);
	end;
	error(index .. " cannot be assigned to", 2);
end;
function i_mt.__tostring(self)
	return (("%s+%si"):format(proxy[self].Real, proxy[self].Imaginary):gsub('%-nan%(ind%)', 'nan'):gsub('%+%-', '-'):gsub('^0%+*', ''));
end;
function i_mt.__eq(self, other)
	return (proxy[self].Real == proxy[other].Real) and (proxy[self].Imaginary == proxy[other].Imaginary);
end;

--[=[ Class methods ]=]--
function methods:Log(base)
	if type(base) == "number" then
		return self:Log() / i.new(base, 0):Log();
	elseif base ~= nil then
		error("invalid argument #2 (number expected, got " .. typeof(base) .. ')', 2);
	end;
	return i.new(math.log(self:Abs()), math.atan2(proxy[self].Imaginary, proxy[self].Real));
end;
function methods:Abs()
	local a, b = proxy[self].Real, proxy[self].Imaginary;
	if b == 0 then
		return math.abs(a);
	elseif a == 0 then
		return math.abs(b);
	elseif a > b then
		return a * math.sqrt(1 + ((b ^ 2)/(a ^ 2)));
	end;
	return b * math.sqrt(1 + ((a ^ 2)/(b ^ 2)));
end;
function methods:Sqrt()
	return i.fromPolarCoordinates(math.sqrt(self.Magnitude), self.Phase / 2);
end;
function methods:Sin()
	return i.new(math.sin(proxy[self].Real) * math.cosh(proxy[self].Imaginary), math.cos(proxy[self].Real) * math.sinh(proxy[self].Imaginary));
end;
function methods:Cos()
	return i.new(math.cos(proxy[self].Real) * math.cosh(proxy[self].Imaginary), -(math.sin(proxy[self].Real) * math.sinh(proxy[self].Imaginary)));
end;
function methods:Tan()
	return self:Sin() / self:Cos();
end;
function methods:Sinh()
	return i.new(math.sinh(proxy[self].Real) * math.cos(proxy[self].Imaginary), math.cosh(proxy[self].Real) * math.sin(proxy[self].Imaginary));
end;
function methods:Cosh()
	return i.new(math.cosh(proxy[self].Real) * math.cos(proxy[self].Imaginary), math.sinh(proxy[self].Real) * math.sin(proxy[self].Imaginary));
end;
function methods:Tanh()
	return self:Sinh() / self:Cosh();
end;
function methods:Asin()
	return i.new(0, 1) * ((i.new(0, 1) * self) + (i.new(1, 0) - (self * self)):Sqrt()):Log();
end;
function methods:Acos()
	return i.new(0, 1) * (i.new(0, 1) + (self * (i.new(1, 0) - (self * self)):Sqrt())):Log();
end;
function methods:Atan()
	return (i.new(0, 1) / i.new(2, 0)) * ((i.new(1, 0) - (i.new(0, 1) * self)):Log() - (i.new(1, 0) + (i.new(0, 1) * self)):Log());
end;
function methods:IsInfinity()
	return (math.abs(proxy[self].Real) == math.huge) or (math.abs(proxy[self].Imaginary) == math.huge);
end;
function methods:IsNaN()
	return (proxy[self].Real ~= proxy[self].Real) or (proxy[self].Imaginary ~= proxy[self].Imaginary);
end;

--[=[ Constructor ]=]--
function i.new(...)
	local argc = select('#', ...);
	if argc < 2 then
		error("missing argument #" .. (argc + 1) .. " (number expected)", 2);
	end;
	local real, imaginary = ...;
	if type(real) ~= "number" then
		error("invalid arguemnt #1 (number expected, got " .. typeof(real) .. ")", 2);
	end;
	if type(imaginary) ~= "number" then
		error("invalid arguemnt #2 (number expected, got " .. typeof(imaginary) .. ")", 2);
	end;
	
	local pointer;
	if newproxy then
		pointer = newproxy(true);
		local mt = getmetatable(pointer);
		for k, f in next, i_mt do
			mt[k] = f;
		end;
	else
		pointer = setmetatable({ }, i_mt);
	end;
	
	proxy[pointer] = {
		Real = real,
		Imaginary = imaginary,
		Magnitude = math.sqrt((real ^ 2) + (imaginary ^ 2)),
		Phase = math.atan2(imaginary, real);
	};
	return pointer;
end;
function i.fromPolarCoordinates(...)
	local argc = select('#', ...);
	if argc < 2 then
		error("missing argument #" .. (argc + 1) .. " (number expected)", 2);
	end;
	local magnitude, phase = ...;
	if type(magnitude) ~= "number" then
		error("invalid arguemnt #1 (number expected, got " .. typeof(magnitude) .. ")", 2);
	end;
	if type(phase) ~= "number" then
		error("invalid arguemnt #2 (number expected, got " .. typeof(phase) .. ")", 2);
	end;
	
	local pointer;
	if newproxy then
		pointer = newproxy(true);
		local mt = getmetatable(pointer);
		for k, f in next, i_mt do
			mt[k] = f;
		end;
	else
		pointer = setmetatable({ }, i_mt);
	end;
	
	proxy[pointer] = {
		Real = magnitude * math.cos(phase),
		Imaginary = magnitude * math.sin(phase),
		Magnitude = magnitude,
		Phase = phase;
	};
	return pointer;
end;

return setmetatable(
	{ },
	{
		__index = function(self, index)
			if i[index] then
				return i[index];
			elseif methods[index] then
				return methods[index];
			end;
			return nil;
		end,
		__metatable = "The metatable is locked",
	}
);
