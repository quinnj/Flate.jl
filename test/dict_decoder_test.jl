using Flate, Test, Flate.Go

@testset "DictDecoder" begin
    abc = "ABC\n"
    fox = "The quick brown fox jumped over the lazy dog!\n"
    poem =
        "The Road Not Taken\nRobert Frost\n" *
        "\n" *
        "Two roads diverged in a yellow wood,\n" *
        "And sorry I could not travel both\n" *
        "And be one traveler, long I stood\n" *
        "And looked down one as far as I could\n" *
        "To where it bent in the undergrowth;\n" *
        "\n" *
        "Then took the other, as just as fair,\n" *
        "And having perhaps the better claim,\n" *
        "Because it was grassy and wanted wear;\n" *
        "Though as for that the passing there\n" *
        "Had worn them really about the same,\n" *
        "\n" *
        "And both that morning equally lay\n" *
        "In leaves no step had trodden black.\n" *
        "Oh, I kept the first for another day!\n" *
        "Yet knowing how way leads on to way,\n" *
        "I doubted if I should ever come back.\n" *
        "\n" *
        "I shall be telling this with a sigh\n" *
        "Somewhere ages and ages hence:\n" *
        "Two roads diverged in a wood, and I-\n" *
        "I took the one less traveled by,\n" *
        "And that has made all the difference.\n"

    poemRefs = [
        (dist=0, length=38), (dist=33, length=3), (dist=0, length=48), (dist=79, length=3), (dist=0, length=11), (dist=34, length=5), (dist=0, length=6), (dist=23, length=7), (dist=0, length=8), (dist=50, length=3), (dist=0, length=2), (dist=69, length=3), (dist=34, length=5), (dist=0, length=4), (dist=97, length=3), (dist=0, length=4), (dist=43, length=5), (dist=0, length=6), (dist=7, length=4), (dist=88, length=7), (dist=0, length=12), (dist=80, length=3), (dist=0, length=2), (dist=141, length=4), (dist=0, length=1), (dist=196, length=3), (dist=0, length=3), (dist=157, length=3), (dist=0, length=6), (dist=181, length=3), (dist=0, length=2), (dist=23, length=3), (dist=77, length=3), (dist=28, length=5), (dist=128, length=3), (dist=110, length=4), (dist=70, length=3), (dist=0, length=4), (dist=85, length=6), (dist=0, length=2), (dist=182, length=6), (dist=0, length=4), (dist=133, length=3), (dist=0, length=7), (dist=47, length=5), (dist=0, length=20), (dist=112, length=5), (dist=0, length=1), (dist=58, length=3), (dist=0, length=8), (dist=59, length=3), (dist=0, length=4), (dist=173, length=3), (dist=0, length=5), (dist=114, length=3), (dist=0, length=4), (dist=92, length=5), (dist=0, length=2), (dist=71, length=3), (dist=0, length=2), (dist=76, length=5), (dist=0, length=1), (dist=46, length=3), (dist=96, length=4), (dist=130, length=4), (dist=0, length=3), (dist=360, length=3), (dist=0, length=3), (dist=178, length=5), (dist=0, length=7), (dist=75, length=3), (dist=0, length=3), (dist=45, length=6), (dist=0, length=6), (dist=299, length=6), (dist=180, length=3), (dist=70, length=6), (dist=0, length=1), (dist=48, length=3), (dist=66, length=4), (dist=0, length=3), (dist=47, length=5), (dist=0, length=9), (dist=325, length=3), (dist=0, length=1), (dist=359, length=3), (dist=318, length=3), (dist=0, length=2), (dist=199, length=3), (dist=0, length=1), (dist=344, length=3), (dist=0, length=3), (dist=248, length=3), (dist=0, length=10), (dist=310, length=3), (dist=0, length=3), (dist=93, length=6), (dist=0, length=3), (dist=252, length=3), (dist=157, length=4), (dist=0, length=2), (dist=273, length=5), (dist=0, length=14), (dist=99, length=4), (dist=0, length=1), (dist=464, length=4), (dist=0, length=2), (dist=92, length=4), (dist=495, length=3), (dist=0, length=1), (dist=322, length=4), (dist=16, length=4), (dist=0, length=3), (dist=402, length=3), (dist=0, length=2), (dist=237, length=4), (dist=0, length=2), (dist=432, length=4), (dist=0, length=1), (dist=483, length=5), (dist=0, length=2), (dist=294, length=4), (dist=0, length=2), (dist=306, length=3), (dist=113, length=5), (dist=0, length=1), (dist=26, length=4), (dist=164, length=3), (dist=488, length=4), (dist=0, length=1), (dist=542, length=3), (dist=248, length=6), (dist=0, length=5), (dist=205, length=3), (dist=0, length=8), (dist=48, length=3), (dist=449, length=6), (dist=0, length=2), (dist=192, length=3), (dist=328, length=4), (dist=9, length=5), (dist=433, length=3), (dist=0, length=3), (dist=622, length=25), (dist=615, length=5), (dist=46, length=5), (dist=0, length=2), (dist=104, length=3), (dist=475, length=10), (dist=549, length=3), (dist=0, length=4), (dist=597, length=8), (dist=314, length=3), (dist=0, length=1), (dist=473, length=6), (dist=317, length=5), (dist=0, length=1), (dist=400, length=3), (dist=0, length=3), (dist=109, length=3), (dist=151, length=3), (dist=48, length=4), (dist=0, length=4), (dist=125, length=3), (dist=108, length=3), (dist=0, length=2),
    ]

    got = IOBuffer()
    want = IOBuffer()
    dd = Flate.DictDecoder()

    Flate.init(dd, 1 << 11, Go.Slice(UInt8, 0))
    writeCopy = ((dist::Int, length::Int) -> begin
        while length > 0
            cnt = Flate.tryWriteCopy(dd, dist, length)
            if cnt == 0
                cnt = Flate.writeCopy(dd, dist, length)
            end
            length -= cnt
            if Flate.availWrite(dd) == 0
                write(got, Flate.readFlush(dd))
            end
        end
    end)

    writeString = ((str::String) -> begin
        while length(str) > 0
            cnt = copy(Flate.writeSlice(dd), str)
            str = str[cnt+1:end]
            Flate.writeMark(dd, cnt)
            if Flate.availWrite(dd) == 0
                write(got, Flate.readFlush(dd))
            end
        end
    end)

    writeString(".")
    write(want, UInt8('.'))
    str = poem
    for ref in poemRefs
        if ref.dist == 0
            writeString(str[begin:ref.length])
        else
            writeCopy(ref.dist, ref.length)
        end
        str = str[ref.length+1:end]
    end

    write(want, poem)

    writeCopy(Flate.histSize(dd), 33)
    write(want, Go.Slice(want)[begin:33])

    writeString(abc)
    writeCopy(length(abc), 59 * length(abc))
    write(want, repeat(abc, 60))

    writeString(fox)
    writeCopy(length(fox), 9 * length(fox))
    write(want, repeat(fox, 10))

    writeString(".")
    writeCopy(1, 9)
    write(want, repeat(".", 10))

    writeString(uppercase(poem))
    writeCopy(length(poem), 7 * length(poem))
    write(want, repeat(uppercase(poem), 8))

    writeCopy(Flate.histSize(dd), 10)
    sl = Go.Slice(want)
    write(want, sl[want.size-Flate.histSize(dd):Go.len(sl)][begin:10])
    write(got, Flate.readFlush(dd))
    @test String(take!(got)) == String(take!(want))
end
