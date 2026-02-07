const std = @import("std");
const fs = std.fs;
const File = std.fs.File;
const FileWriter = std.fs.File.Writer;
const Writer = std.Io.Writer;
const Allocator = std.mem.Allocator;
const sqrt = std.math.sqrt;

const Rgb = struct {
    r: u8 = 0,
    g: u8 = 0,
    b: u8 = 0,

    pub fn scale(self: Rgb, val: f32) Rgb {
        return .{
            .r = @intFromFloat(@as(f32, @floatFromInt(self.r)) * val),
            .g = @intFromFloat(@as(f32, @floatFromInt(self.g)) * val),
            .b = @intFromFloat(@as(f32, @floatFromInt(self.b)) * val),
        };
    }

    pub fn arrayMean(array: []const Rgb) Rgb {
        if (array.len == 0) return .{};

        var r: f32 = 0;
        var g: f32 = 0;
        var b: f32 = 0;
        const float_len = @as(f32, @floatFromInt(array.len));

        for (array) |color| {
            r += @as(f32, @floatFromInt(color.r)) / float_len;
            g += @as(f32, @floatFromInt(color.g)) / float_len;
            b += @as(f32, @floatFromInt(color.b)) / float_len;
        }

        return .{
            .r = @intFromFloat(r),
            .g = @intFromFloat(g),
            .b = @intFromFloat(b),
        };
    }
};

const Ppm = struct {
    w: usize,
    h: usize,
    pixels: []Rgb,

    pub fn init(w: usize, h: usize, alloc: Allocator) !Ppm {
        const pixels = try alloc.alloc(Rgb, h*w);
        @memset(pixels, .{ .r = 0, .g = 0, .b = 0 });
        return .{
            .h = h,
            .w = w,
            .pixels = pixels,
        };
    }

    pub fn deinit(self: *Ppm, alloc: Allocator) void {
        alloc.free(self.pixels);
    }

    pub fn set(self: *Ppm, ix: usize, iy: usize, color: Rgb) !void {
        self.pixels[iy * self.w + ix] = color;
    }

    pub fn save(self: *Ppm) !void {
        var buffer: [1024]u8 = undefined;

        const file: File = try fs.cwd().createFile("out.ppm", .{});
        defer file.close();

        // file_writer needs to live at least as long as writer
        var file_writer: FileWriter = file.writer(&buffer);
        const writer: *Writer = &file_writer.interface;

        try writer.print("P6\n", .{});
        try writer.print("{d} {d}\n", .{self.w, self.h});
        try writer.print("255\n", .{});
        for (self.pixels) |color| {
            try writer.writeByte(color.r);
            try writer.writeByte(color.g);
            try writer.writeByte(color.b);
        }
        try writer.flush();
    }
};

const P3 = struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,

    pub fn dot(self: P3, p: P3) f32 {
        return self.x * p.x + self.y * p.y + self.z * p.z;
    }

    pub fn sum(self: P3, p: P3) P3 {
        return .{
            .x = self.x + p.x,
            .y = self.y + p.y,
            .z = self.z + p.z,
        };
    }

    pub fn sub(self: P3, p: P3) P3 {
        return .{
            .x = self.x - p.x,
            .y = self.y - p.y,
            .z = self.z - p.z,
        };
    }

    pub fn mul(self: P3, scalar: f32) P3 {
        return .{
            .x = self.x * scalar,
            .y = self.y * scalar,
            .z = self.z * scalar,
        };
    }

    pub fn div(self: P3, scalar: f32) P3 {
        return .{
            .x = self.x / scalar,
            .y = self.y / scalar,
            .z = self.z / scalar,
        };
    }

    pub fn norm(self: P3) P3 {
        const len = @sqrt(self.dot(self));
        return self.div(len);
    }
};

const Ray = struct {
    origin: P3 = .{},    // origin
    dir: P3 = .{},       // direction
};

const Sphere = struct {
    cent: P3,
    rad: f32,
    color: Rgb,
};

const HitInfo = struct {
    t: f32 = std.math.inf(f32),
    p: P3 = .{},
    n: P3 = .{},
    hit: bool = false,
    color: Rgb = .{},
};

fn hit_sphere(r: Ray, s: Sphere) HitInfo {
    // coefficients for quadratic equation A t^2 + B t + C = 0
    const A: f32 = r.dir.dot(r.dir);
    const B: f32 = r.dir.dot(r.origin.sub(s.cent)) * 2;
    const C: f32 = (r.origin.sub(s.cent)).dot(r.origin.sub(s.cent)) - (s.rad * s.rad);

    const delta: f32 = B * B - 4 * A * C;
    if (delta < 0) return HitInfo{};

    // choose smaller root first (closest intersection)
    var t: f32 = (-B - sqrt(delta)) / (2 * A);
    if( t <= 0 ) t = (-B + sqrt(delta)) / (2 * A);
    // if still non-positive, intersection is behind ray origin
    if (t <= 0) return HitInfo{};

    // fill hit information
    const p = r.origin.sum(r.dir.mul(t));
    return HitInfo{
        .t = t,
        .p = p,
        .n = p.sub(s.cent).norm(),
        .color = s.color,
        .hit = true,
    };
}

fn max(a: f32, b: f32) f32 {
    if (a >= b) return a;
    return b;
}

pub fn main() !void {
    const number_of_rays = 10;
    // Use std.heap.GeneralPurposeAllocator(.{}).init for faster Allocations
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer switch (gpa.deinit()) {
        std.heap.Check.leak => std.debug.print("Attention! Leaks Found\n", .{}),
        std.heap.Check.ok => std.debug.print("No Leaks Found\n", .{}),
    };
    const alloc: Allocator = gpa.allocator();
    var prng = std.Random.DefaultPrng.init(634318);
    const rand = prng.random();
 
    var image = try Ppm.init(800, 800, alloc);
    defer image.deinit(alloc);

    const eye: P3 = P3{ .x = 0, .y = 0, .z = 0};
    const scene = [_]Sphere{
        .{
            .cent = .{ .x = 0, .y = 0, .z = -3},
            .rad = 1,
            .color = .{ .r = 255, .g = 0, . b = 0}
        },
        .{
            .cent = .{ .x = 0.6, .y = 0.6, .z = -2},
            .rad = 0.2,
            .color = .{ .r = 0, .g = 0, . b = 255}
        },
    };

    const light: P3 = .{ .x = 1, .y = 1, .z = -1};
    var xoffsets: [number_of_rays]f32 = undefined;
    for(&xoffsets) |*offset| offset.* = rand.float(f32);
    var yoffsets: [number_of_rays]f32 = undefined;
    for(&yoffsets) |*offset| offset.* = rand.float(f32);

    const aspect: f32 = @as(f32, @floatFromInt(image.w)) / @as(f32, @floatFromInt(image.h));

    for (0..image.w) |ix| {
        for (0..image.h) |iy| {
            var colors = [_]Rgb{.{}} ** number_of_rays;
            for (&colors, xoffsets, yoffsets) |*color, xoffset, yoffset| {
                const pixpos: P3 = .{
                    .x = (-1 + 2 * (@as(f32, @floatFromInt(ix)) + xoffset) / @as(f32, @floatFromInt(image.w))) * aspect,
                    .y = 1 - 2 * (@as(f32, @floatFromInt(iy)) + yoffset) / @as(f32, @floatFromInt(image.h)),
                    .z = -1
                };
                const r: Ray = .{
                    .origin = eye,
                    .dir = pixpos.sub(eye),
                }; // primary ray

                var best_hi: HitInfo = .{}; // best intersection so far
                for (scene) |elem| {
                    const hi = hit_sphere(r, elem); // test intersection
                    if (hi.t < best_hi.t) { // closer hit found
                        best_hi = hi;
                        const p: P3 = r.origin.sum(r.dir.mul(hi.t)); // hit point
                        const L: P3 = light.sub(p).norm(); // vector to light

                        // offset origin slightly to avoid self-intersection (shadow acne)
                        const shadow_ray: Ray = .{ 
                            .origin = p.sum(L.div(1000)), 
                            .dir = L
                        };

                        // check for occlusion: if any object blocks the light, point is in shadow
                        var shadowed = false;
                        for (scene) |elem1| {
                            shadowed = hit_sphere(shadow_ray, elem1).hit;
                            if (shadowed) break;
                        }

                        if (!shadowed) { // light visible -> simple Lambertian shading
                            const cosLN: f32 = hi.n.dot(L);
                            const al: f32 = max(0, cosLN); // clamp negative values
                            color.* = hi.color.scale(al); // scale object color by diffuse term
                        }
                    }
                }
            }

            try image.set(ix, iy, Rgb.arrayMean(&colors));
        }
    }

    try image.save();
}
