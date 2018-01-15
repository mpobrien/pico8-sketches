pico-8 cartridge // http://www.pico-8.com
version 15
__lua__

vec3d = {}
cam = {}

cube = {}

function cube:new()
  local newobj = {}
  self.data = {
	{x= 1,  y= 1,  z= 1},
	{x= -1, y= 1,  z= 1},
	{x= -1, y= -1, z= 1},
	{x= 1,  y= -1, z= 1},
	{x= 1,  y= 1,  z= -1},
	{x= -1, y= 1,  z= -1},
	{x= -1, y= -1, z= -1},
	{x= 1,  y= -1, z= -1},
  }
  self.edges = {
	  {1, 2},
	  {2, 3},
	  {3, 4},
	  {4, 1},
	  {5, 6},
	  {6, 7},
	  {7, 8},
	  {8, 5},
	  {1, 5},
	  {2, 6},
	  {3, 7},
	  {4, 8}
  }

  self.__index = self
  return setmetatable(newobj, self)
end

function cube:move(dx, dy, dz)
  print(self.data[0])
  for i = 1, #self.data do
	  self.data[i].x += dx
	  self.data[i].y += dy
	  self.data[i].z += dz
	end
end

function cube:update(theta)
	--local center 
	--center = {x=0, y=0}
	--for i=1, #self.data do
		--self.data[i].x = center.x + (self.data[i].x-center.x)*cos(theta) - (self.data[i].y-center.y)*sin(theta);
		--self.data[i].y = center.y + (self.data[i].x-center.x)*sin(theta) + (self.data[i].y-center.y)*cos(theta);
	--end
end



function cube:draw(cam, dx, dy, dz)
	for i=1, #self.edges do
		local point1, point2 = self.data[self.edges[i][1]], self.data[self.edges[i][2]]
		local z1, z2 = point1.z - c.pos.z, point2.z - c.pos.z
		local f1, f2 = 128 / z1, 128 / z2
		local x1, x2 = (point1.x - c.pos.x) * f1, (point2.x - c.pos.x) * f2
		local y1, y2 = (point1.y - c.pos.y) * f1, (point2.y - c.pos.y) * f2
		line(x1+cx, y1+cy, x2+cx, y2+cy, 6)
	end
end

function cam:new(pos, velocity, rot)
	local newcam = {pos=pos, velocity=velocity, rot=rot}
    self.__index = self
	return setmetatable(newcam, self)
end

function cam:update()
	--self.pos = self.pos:plus(self.velocity)
	if(btn(0)) self.pos.x -= .1
	if(btn(1)) self.pos.x += .1
	if(btn(2)) self.pos.y -= .1
	if(btn(3)) self.pos.y += .1
	if(btn(4)) self.rot.y -= .01
	if(btn(5)) self.rot.y += .01
	
	--newy, newz = rotate2d(self.pos.y, self.pos.z, 0,1, 0.001)
	--self.pos.x = newx
	--self.pos.y = newy
	--self.pos.z = newz
	
end

function vec3d:new(x, y, z)
	local newvec = {x=x, y=y, z=z}
    self.__index = self
	return setmetatable(newvec, self)
end

function vec3d:plus(other) -- other must be a vector
	return vec3d:new(other.x+self.x, other.y+self.y, other.z+self.z)
end

function vec3d:minus(other) -- other must be a vector
	return vec3d:new(self.x-other.x, self.y-other.y, self.z-other.z)
end

function vec3d:mult(by) -- by must be a scalar
	return vec3d:new(self.x*by, self.y*by, self.z*by)
end

--function vec3d:__div(by) -- by must be a scalar
	--return vec3d:new(self.x/by, self.y/by, self.z/by)
--end

--function vec3d:unit():
	--local mag = self.mag()
	--return self / mag
--end

function vec3d:dot(other)
	return (self.x *other.x) + (self.y*other.y) + (self.z*other.z)
end

function vec3d:mag()
	return sqrt(self.x*self.x + self.y*self.y + self.z*self.z)
end

function vec3d:cross(other)
	-- u x v =  |u2 u3|i - |u1 u3|j + |u1 u2|k
	--          |v2 v3|    |v1 v3|    |v1 v2|

    -- (u2v3 - u3v2)i - (u1v3 - u3v1)j + (u1v2 - u2v1)k
	return vec3d:new(   
		(self.y * other.z) - (self.z * other.y),
		-(self.x * other.z - self.z * other.x),
		(self.x * other.y - self.y * other.x)
	)
end

function vec3d:string()
	return "x=".. self.x ..",y=".. self.y .. ",z=" .. self.z
end



--function cube:rotate(theta)
	--local center 
	--center = {x=0, y=0}
	--for i=1, #self.data do
		--self.data[i].x = center.x + (self.data[i].x-center.x)*cos(theta) - (self.data[i].y-center.y)*sin(theta);
		--self.data[i].y = center.y + (self.data[i].x-center.x)*sin(theta) + (self.data[i].y-center.y)*cos(theta);
	--end
--end
--
--function cube:draw(dx, dy, dz)
	--for i=1, #self.edges do
		--local point1, point2 = self.data[self.edges[i][1]], self.data[self.edges[i][2]]
		--local z1, z2 = point1.z + 4, point2.z + 4
		--local f1, f2 = 128 / z1, 128 / z2
		--local x1, x2 = point1.x * f1, point2.x * f2
		--local y1, y2 = point1.y * f1, point2.y * f2
		--line(x1+cx, y1+cy, x2+cx, y2+cy)
		---- circ(cx + x, cy+ y, 3)
	--end
--end

w = 128
h = 128
cx = w/2
cy = h/2
c = 1

points = {
	vec3d:new(1, 1, 1)
}

function rotate2d(x,y,cx,cy,angle)
	local sina=sin(angle)
	local cosa=cos(angle)
	x-=cx
	y-=cy
	local rotx=cosa*x-sina*y
	local roty=sina*x+cosa*y
	rotx+=cx
	roty+=cy
	
	return rotx,roty
end

function _init()
	local v1 = vec3d:new(3, -3, 1)
	local v2 = vec3d:new(4, 9, 2)
	local d = v1:cross(v2)
end

function comparator(a, b)
	return a.pos.z - b.pos.z
end

ticker = 0
function _update()
	ticker += 0.01
	c:update()
	for i=1, #objs do
		objs[i]:update()
	end
	heapsort(objs, comparator)
	--local theta = .005
	--for i=1, #points do
		----print(points[i]:string())
		--local rotx, roty = rotate2d(points[i].x, points[i].y, 0, 0, theta)
		--points[i] = vec3d:new(rotx, roty, points[i].z)
	--end
end

orb = {}
function orb:new(pos, col)
	local neworb = {pos=pos, col=col}
    self.__index = self
	return setmetatable(neworb, self)
end

function orb:update(c)
	local center = vec3d:new(0,0,0)
	self.pos.x = sin(self.phase)
	self.pos.z = cos(self.phase)
	self.phase += 0.01
	--local newx = center.x + (self.pos.x-center.x)*cos(theta) - (self.pos.y-center.y)*sin(theta);
	--local newz = center.z + (self.pos.z-center.z)*cos(theta) - (self.pos.y-center.y)*sin(theta);
	--local newy = center.y + (self.pos.x-center.x)*sin(theta) + (self.pos.y-center.y)*cos(theta);
	--self.pos.z = newz
	--self.pos.y = newy
end

function orb:draw(c)
	--local newx, newy, newz
	--newx, newz = rotate2d(self.pos.x, self.pos.z, 0,0, 0.001)
	--newy, newz = rotate2d(self.pos.y, newz, 0,0, 0.001)
	--self.pos.x = newx
	--self.pos.y = newy
	--self.pos.z = newz

	local x, y, z
	x = self.pos.x - c.pos.x
	y = self.pos.y - c.pos.y
	z = self.pos.z - c.pos.z
	x, z = rotate2d(x, z, 0, 0, c.rot.x)
	y, z = rotate2d(y, z, 0, 0, c.rot.y)
	local f = 128 / z
	x *= f
	y *= f
	local f1 = f/3
	fillp(0b0000000000000000)
	circfill(cx + x, cy+ y, f1, self.col)
	circ(cx + x, cy+ y, f1, 0)
end

c = cam:new(vec3d:new(4,3,-9), vec3d:new(0, 0, 0), vec3d:new(.1,.1,0))
objs = {
	--cube:new(),
	--orb:new(vec3d:new(1, -1,     1), 9),
	--orb:new(vec3d:new(1.25, -1.25,  1), 9),
	--orb:new(vec3d:new(1.5, -1.5,   1), 9),
	--orb:new(vec3d:new(1.25, -1.75,  1), 9),
	--orb:new(vec3d:new(1.3, -2,     1), 9),
	--orb:new(vec3d:new(4, -2.25,  1), 9),
	--orb:new(vec3d:new(6, -2.5,   1), 9),
}

for i=1, 100 do
	local col = 3 + (9*(i % 2))
	local phase = i / 25
	local x, y, z = sin(phase), i * .4, cos(phase)
	local o = orb:new(vec3d:new(x, y, z), col)
	o.phase = phase
	add(objs, o)
	x, y, z = sin(phase + 0.5), i * .4, cos(phase + 0.5) 
	o = orb:new(vec3d:new(x, y, z), col)
	o.phase = phase + 0.5
	add(objs, o)
end

function _draw()
	cls()
	fillp(0b0011001111001100)
	rectfill(0,0,256,256, 1)


	for i=1, #objs do
		objs[i]:draw(c)
	end
end

function heapsort(t, cmp)
 local n = #t
 local i, j, temp
 local lower = flr(n / 2) + 1
 local upper = n
 while 1 do
  if lower > 1 then
   lower -= 1
   temp = t[lower]
  else
   temp = t[upper]
   t[upper] = t[1]
   upper -= 1
   if upper == 1 then
    t[1] = temp
    return
   end
  end
  i = lower
  j = lower * 2
  while j <= upper do
   if j < upper and cmp(t[j], t[j+1]) < 0 then
    j += 1
   end
   if cmp(temp, t[j]) < 0 then
    t[i] = t[j]
    i = j
    j += i
   else
    j = upper + 1
   end
  end
  t[i] = temp
 end
end



__sfx__
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001300000015300000000000000000000000000015300153000000015300000001530000000153000000000000153000000000000000000000000000053000530000000053000000005300000000000000000000
001300000e1061110615106180001065500005071000b10602106051060000500005106450c0060f00616006000051a0001a00000005106450000500005000050000500005000050000510645000050000500015
001300000c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c0030c003
001300000c0250c0250c0250c0250e0250e0250e0250e0250e0250e0250e0250e025100251002510025100251302513025130251302513025100251002510025100251002510025100250c0250c0250c0250c025
__music__
03 01020304

