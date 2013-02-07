package darts.lib.json.write

import java.io.Writer
import java.io.StringWriter
import java.io.OutputStreamWriter
import java.io.OutputStream

private sealed abstract class State {

    protected def badState(s: String): Nothing = throw new IllegalStateException("'" + s + "' in " + getClass.getSimpleName)
    
    def value[T](writer: Renderer, value: T, formatter: Formatter[T]): State = badState("value")
    def key(writer: Renderer, key: String): State = badState("key")
    def openMap(writer: Renderer): State = badState("openMap")
    def closeMap(writer: Renderer): State = badState("closeMap")
    def openArray(writer: Renderer): State = badState("openArray")
    def closeArray(writer: Renderer): State = badState("closeArray")
}

private final class ArrayFirst (val next: State) extends State {
    
    override def value[T](writer: Renderer, value: T, formatter: Formatter[T]): State = {
        writer.write("[")
        formatter.format(writer, value)
        new ArrayNext(next)
    } 

    override def openMap(writer: Renderer): State = {
        writer.write("[")
        new MapFirstKey(new ArrayNext(next))
    }

    override def openArray(writer: Renderer): State = {
        writer.write("[")
        new ArrayFirst(new ArrayNext(next))
    }
    
    override def closeArray(writer: Renderer): State = {
        writer.write("[]")
        next
    }
}

private final class ArrayNext (val next: State) extends State {
    
    override def value[T](writer: Renderer, value: T, formatter: Formatter[T]): State = {
        writer.write(", ")
        formatter.format(writer, value)
        this
    }

    override def openMap(writer: Renderer): State = {
        writer.write(", ")
        new MapFirstKey(this)
    }

    override def openArray(writer: Renderer): State = {
        writer.write(", ")
        new ArrayFirst(this)
    }
    
    override def closeArray(writer: Renderer): State = {
        writer.write("]")
        next
    }
}

private final class MapFirstKey (val next: State) extends State {
    
    override def closeMap(writer: Renderer): State = {
        writer.write("{}")
        next
    }
    
    override def key(writer: Renderer, key: String): State = {
        writer.write("{")
        Formatter.writeString(writer, key)
        writer.write(": ")
        new MapValue(next)
    }
}

private final class MapKey(val next: State, val vstate: State) extends State {
    
    override def closeMap(writer: Renderer): State = {
        writer.write("}")
        next
    }
    
    override def key(writer: Renderer, key: String): State = {
        writer.write(", ")
        Formatter.writeString(writer, key)
        writer.write(": ")
        vstate
    }
}

private final class MapValue(val next: State) extends State {
    
    private lazy val keyState = new MapKey(next, this)
    
    override def value[T](writer: Renderer, value: T, formatter: Formatter[T]): State = {
        formatter.format(writer, value)
        keyState
    }

    override def openMap(writer: Renderer): State = {
        new MapFirstKey(keyState)
    }
    
    override def closeMap(writer: Renderer): State = {
        badState("closeMap")
    }

    override def openArray(writer: Renderer): State = {
        new ArrayFirst(keyState)
    }
}

private object State {
    
    final object Invalid extends State
    
    final object TopLevel extends State {
        override def value[T](writer: Renderer, value: T, formatter: Formatter[T]): State = {
            formatter.format(writer, value)
            Invalid
        }
        override def openArray(writer: Renderer): State = new ArrayFirst(Invalid)
        override def openMap(writer: Renderer): State = new MapFirstKey(Invalid)
    }
}

abstract class Renderer { outer =>
	
    private var state: State = State.TopLevel
    
    /**
     * This method is provided for the benefit of formatters,
     * which need to render complex objects. The state handling
     * machinery does not allow them to call `withMap` or `withList`
     * directly on the renderer they were given. If they wish to
     * render complex objects, they must do so in a sub-renderer
     * provided by this method.
     * 
     * In other words: any formatter (and only a formatter), which
     * wants to call one of the following methods to perform its
     * job:
     * 
     * - value
     * - openMap (or withMap)
     * - openArray (or withList)
     * 
     * must do so in a sub-renderer from within the dynamic 
     * extent of a call to this method.
     */

    def sub[U](fn: Renderer=>U): U = fn(new Renderer {
        def close(): Unit = throw new UnsupportedOperationException
        def write(str: String, start: Int, len: Int): Unit = outer.write(str, start, len)
    })

    /**
     * Close this renderer, also closing its underlying output
     * stream if necessary.
     */
    
    def close(): Unit
    
    /**
     * Write some literal string into the renderer's underlying
     * output stream. Note, that this method does not escape the
     * data from `str` in any way, assuming, that it contains
     * already pre-rendered JSON.
     * 
     * This method writes the portion of `str`, which starts at
     * index `start` (inclusive) and spans the next `len` characters.
     * The effect is undefined, if `start` is below 0 or greater
     * than `str.length`, or if `len` is negative or `start + len`
     * is greater than `str.length`. The most likely effect is
     * an exception being thrown in these cases.
     * 
     * @param str	text to write
     * @param start	start index 
     * @param len	character count 
     */
    
    def write(str: String, start: Int, len: Int): Unit
    
    /**
     * Write some literal string into the renderer's underlying
     * output stream. Note, that this method does not escape the
     * data from `str` in any way, assuming, that it contains
     * already pre-rendered JSON.
     * 
     * @param str	text to write
     */
    
    def write(str: String): Unit = write(str, 0, str.length)
    
    def value[T](value: T)(implicit fmt: Formatter[T]): this.type = {
        state = state.value(this, value, fmt)
        this
    }
    
    def undefined: this.type = {
        state = state.value(this, "", Renderer.NullFmt)
        this
    }
    
    def withMap[U](fn: =>U): this.type = {
        openMap; fn; closeMap
    }
    
    def withList[U](fn: =>U): this.type = {
        openList; fn; closeList
    }
    
    def openMap: this.type = {
        state = state.openMap(this)
        this
    }
    
    def closeMap: this.type = {
        state = state.closeMap(this)
        this
    }
    
    def key(str: String): this.type = {
        state = state.key(this, str)
        this
    }
    
    def openList: this.type = {
        state = state.openArray(this)
        this
    }
    
    def closeList: this.type = {
        state = state.closeArray(this)
        this
    }
}

final class WriterRenderer (val writer: Writer) extends Renderer {
    
    def close(): Unit = writer.close()    
    def write(str: String, start: Int, len: Int): Unit = writer.write(str, start, len)
}

private final class BorrowedOutStream (val underlying: OutputStream) extends OutputStream {
    override def close() = underlying.flush()
    override def flush() = underlying.flush()
    override def write(buf: Array[Byte]) = underlying.write(buf)
    override def write(buf: Array[Byte], start: Int, len: Int) = underlying.write(buf, start, len)
    override def write(octet: Int) = underlying.write(octet)
}

private final class BorrowedWriter (val underlying: Writer) extends Writer {
    override def close() = underlying.flush()
    override def flush() = underlying.flush()
    override def write(buf: Array[Char]) = underlying.write(buf)
    override def write(buf: Array[Char], start: Int, len: Int) = underlying.write(buf, start, len)
    override def write(ch: Int) = underlying.write(ch)
    override def write(str: String) = underlying.write(str)
    override def write(str: String, start: Int, len: Int) = underlying.write(str, start, len)
}

object Renderer {
    
    private def doRender[U](renderer: Renderer, fn: Renderer=>U): Unit = {
        fn(renderer)
        renderer.close
    }
    
    /**
     * Construct a renderer, which generates its output into the
     * given `writer`. Note, that closing the renderer will *not* 
     * close the writer; the caller remains fully in charge of it.
     * 
     * @param writer	writer to be filled
     * @param fn		thunk to actually generate the JSON
     */
    
    def rendering[U](writer: Writer)(fn: Renderer=>U): Unit = {
        doRender(new WriterRenderer(new BorrowedWriter(writer)), fn)
    }

    /**
     * Construct a renderer, which generates its output into the
     * given `stream`. Note, that closing the renderer will *not* 
     * close the stream; the caller remains fully in charge of it.
     * 
     * @param stream	stream to write into
     * @param encoding	names the character encoding to use
     * @param fn		thunk to actually generate the JSON
     */
    
    def rendering[U](stream: OutputStream, encoding: String)(fn: Renderer=>U): Unit = {
        doRender(new WriterRenderer(new OutputStreamWriter(new BorrowedOutStream(stream), encoding)), fn)
    }
    
    /**
     * Construct a renderer, which generates its output into the
     * given `stream`. Note, that closing the renderer will *not* 
     * close the stream; the caller remains fully in charge of it.
     * 
     * @param stream	stream to write into
     * @param fn		thunk to actually generate the JSON
     */
    
    def rendering[U](stream: OutputStream)(fn: Renderer=>U): Unit = {
        doRender(new WriterRenderer(new OutputStreamWriter(new BorrowedOutStream(stream))), fn)
    }
    
    /**
     * Constructs a temporary renderer and passes it to `fn`, which
     * is supposed to actually write some JSON values into it. The
     * result is collected into a buffer, and returned as string from 
     * this function, after `fn` returns.
     * 
     * @return	a string containing the JSON representation of 
     * 		   	whatever `fn` wrote
     */
    
    def stringify[U](fn: Renderer=>U): String = {
        val w = new StringWriter
        doRender(new WriterRenderer(w), fn)
        w.toString
    } 
    
    private object NullFmt extends Formatter[Any] {
        def format(r: Renderer, a: Any): Unit = r.write("null")
    } 
}
