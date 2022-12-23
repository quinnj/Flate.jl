using Flate.GoTypes, Test

# Test GoArray
@testset "GoArray" begin

    x = Go.Array(Int, 10)
    @test length(x) == 10
    @test x[0] == 0
    @test x[9] == 0
    @test_throws BoundsError x[10]
    @test_throws BoundsError x[-1]
    x[0] = 1
    x[9] = 2
    @test x[0] == 1
    @test x[9] == 2

    x = Go.Array(Int, 1, 2, 3)
    @test length(x) == 3
    @test x[0] == 1
    @test x[1] == 2
    @test x[2] == 3
    @test_throws BoundsError x[3]

    @test x == Go.Array(1, 2, 3)
end

# Test GoSlice
@testset "GoSlice" begin

    x = Go.Array(Int, 10)
    x[0] = 1
    x[9] = 2
    s = x[0:10]
    @test s == x[:]
    @test length(s) == 10
    @test s[0] == 1
    @test s[9] == 2
    @test_throws BoundsError s[10]
    @test_throws BoundsError s[-1]
    s[0] = 3
    s[9] = 4
    @test s[0] == 3
    @test s[9] == 4
    @test x[0] == 3
    @test x[9] == 4

    # test GoSlice with offset
    s = x[1:9]
    @test length(s) == 8
    @test s[0] == 0
    @test s[7] == 0
    @test_throws BoundsError s[8]
    @test_throws BoundsError s[-1]
    s[0] = 5
    s[7] = 6
    @test s[0] == 5
    @test s[7] == 6
    @test x[1] == 5
    @test x[8] == 6

    # test indexing GoSlice produces GoSlice
    ss = s[1:7]
    @test length(ss) == 6
    @test ss[0] == 0
    @test ss[5] == 0
    ss[0] = 7
    ss[5] = 8
    @test x[2] == s[1] == ss[0] == 7
    @test x[7] == s[6] == ss[5] == 8

    # test reslicing beyond original slice length
    sss = ss[0:8]
    @test length(sss) == 8
    @test sss[0] == 7
    @test sss[7] == 4

    # test dynamic GoArray generation via GoSlice constructor
    s = Go.Slice(Int, 10)
    @test length(s) == 10
    @test s[0] == 0
    @test s[9] == 0
    @test_throws BoundsError s[10]
    @test_throws BoundsError s[-1]

    # test GoSlice with cap
    s = Go.Slice(Int, 10, 20)
    @test length(s) == 10
    @test s[0] == 0
    @test s[9] == 0
    @test_throws BoundsError s[10]
    @test Go.cap(s) == 20

    # slice of slices works
    s1 = Go.Slice(0, 1, 2, 3, 4)
    s = Go.Slice(s1)
    @test s[0][4] == 4

    # test copy between GoSlices
    s1 = Go.Slice(0, 1, 2, 3, 4)
    s2 = Go.Slice(5, 6, 7, 8, 9)
    copy(s1, s2)
    @test s1 == s2

    # test append to GoSlice
    s1 = Go.Slice(0, 1, 2, 3, 4)
    s1 = Go.append(s1, 5, 6)
    @test s1 == Go.Slice(0, 1, 2, 3, 4, 5, 6)
    x = Go.Array(0, 1, 2, 3, 4)
    s = x[1:2]
    # there is enough space, so this doesn't allocate
    # a new GoArray
    s1 = Go.append(s, 5, 6, 7)
    s1[0] = 10
    @test s1[0] == x[1] == 10

    x = Go.Array(0, 1, 2, 3, 4)
    # slice of full array
    s = x[0, :]
    @test s == x
    # if we try to use end, it selects 1 less
    # than underlying array because slice
    # expression is half-open
    s = x[0:end]
    @test s == x[0:4]
    # using [lo:] on slice, doesn't extend slice
    # just selects "rest of slices"
    @test s[0, :] == s
end
