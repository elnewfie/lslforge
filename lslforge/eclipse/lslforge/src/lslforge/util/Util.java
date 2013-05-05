package lslforge.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import lslforge.LSLForgePlugin;
import lslforge.generated.Ctx;
import lslforge.generated.Ctx_Ctx;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;

public class Util {
   
    /**
     * Find the absolute offset in a file of a give line/column.  Lines start at 0, and
     * columns include the notion of a tab stop, where there is a tab stop every 8 columns. So
     * the 4th character on a line could be at column 24, if the first 3 characters are tabs.  If
     * the first two characters are non-tabs, and the third character is a tab, then the 4th
     * character is at column 8.  But if the first character is a tab, the next two are non tabs,
     * then the 4th character is at column 10.  Etc.  This convoluted algorithm is necessary because
     * Eclipse annotations are based on absolute start and end character offsets in a file, but
     * the parser currently used for parsing LSL has a notion of lines and columns, which includes
     * tab stops in its column calculations.  As a future upgrade, one might envision hacking the
     * underlying parser library (Haskell's Parsec) to include absolute file position in addition
     * to line/column (because the whole line column thing, in addition to requiring the convoluted
     * code below, also means we have to reread in Java every file that we originally read in
     * in the compiler.  Which is unfortunate.).
     * @param lines the line offset
     * @param columns the column offset
     * @param f the file
     * @return the offsets.
     */
    public static int[] findOffsetsFor(int[] lines, int[] columns, IFile f) {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(f.getContents()));
            
            int index = 0;
            int lineIndex = 0;
            int colIndex = 0;
            int[] result = new int[lines.length];
            for (int i = 0; i < lines.length; i++) {
                while (lineIndex < lines[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        colIndex = 0;
                    }
                    index++;
                }
                
                boolean incIndex = false;
                while (colIndex < columns[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        incIndex = true;
                        colIndex = 0;
                        break;
                    }
                    if (ch == '\t') colIndex += (8 - colIndex % 8);
                    else colIndex += 1;
                    index++;
                }
                result[i] = index;
                if (incIndex) index++;
            }
            return result;
        } catch (Exception e) {
            Log.error(e);
            return null;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                }
            }
        }
        
    }
    
    public static int[] findOffsetsFor(int[] lines, int[] columns, String f) {
        StringReader reader = null;
        try {
            reader = new StringReader(f);
            
            int index = 0;
            int lineIndex = 0;
            int colIndex = 0;
            int[] result = new int[lines.length];
            for (int i = 0; i < lines.length; i++) {
                while (lineIndex < lines[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        colIndex = 0;
                    }
                    index++;
                }
                
                boolean incIndex = false;
                while (colIndex < columns[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        incIndex = true;
                        colIndex = 0;
                        break;
                    }
                    if (ch == '\t') colIndex += (8 - colIndex % 8);
                    else colIndex += 1;
                    index++;
                }
                result[i] = index;
                if (incIndex) index++;
            }
            return result;
        } catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

    public static List<Integer> compulteLineOffsets(IFile f) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(f.getContents()));
            
            ArrayList<Integer> list = new ArrayList<Integer>();
            
            list.add(new Integer(0));
            int ch;
            int index = 0;
            while ((ch = reader.read()) >= 0) {
                index++;
                if (ch == '\n') {
                    list.add(new Integer(index));
                }
            }
            list.add(new Integer(index));
            
            return list;
        } catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

	public static Object[] append(Object[] lst,Object[] lst1) {
		Object[] l = (Object[]) Array.newInstance(lst.getClass().getComponentType(), lst.length + lst1.length);
		System.arraycopy(lst, 0, l, 0, lst.length);
		System.arraycopy(lst1, 0, l, lst.length, lst1.length);
		return l;
	}
	
	public static Object[] concat(Object[][] lsts) {
		int totLength = 0;
		Class<?> componentType = null;
		
		for (int i = 0; i < lsts.length; i++) {
			Object[] lst = lsts[i];
			if (lst == null) continue;
			if (componentType == null) componentType = lst.getClass().getComponentType();
			else {
				Class<?> newComponentType = lst.getClass().getComponentType();
				
				if (newComponentType != componentType) {
					if (!componentType.isAssignableFrom(newComponentType)) {
						if (newComponentType.isAssignableFrom(componentType)) {
							componentType = newComponentType;
						} else {
							// punt!
							componentType = Object.class;
						}
					}
				}
			}
			
			totLength += lst.length;
		}
		
		if (componentType == null) {
			return new Object[0];
		}
		Object[] l = (Object[]) Array.newInstance(componentType, totLength);
		int offset = 0;
		for (int i = 0; i < lsts.length; i++) {
			Object[] lst = lsts[i];
			if (lst == null) continue;
			
			System.arraycopy(lst, 0, l, offset, lst.length);
			offset += lst.length;
		}
		
		return l;
	}
	
	public static interface ArrayMapFunc<T> {
		public T map(Object o);
		public Class<T> elementType();
	}
	
	public static interface MapFunc<S,T> {
		public T map(S o);
		public Class<T> elementType();
	}
	
	public static interface Predicate {
		public boolean test(Object o);
	}
	
	public static Object find(Predicate p, Object[] list) {
		for (int i = 0; i < list.length; i++) {
			if (p.test(list[i])) return list[i];
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T[] arrayMap(ArrayMapFunc<T> f, Object[] list) {
		T[] o = (T[]) Array.newInstance(f.elementType(), list.length);
		
		for (int i = 0; i < list.length; i++) {
			o[i] = f.map(list[i]);
		}
		
		return o;
	}
	
	public static <S,T> T[] listMapToArray(MapFunc<S,T> f, List<S> list) {
		@SuppressWarnings("unchecked")
		T[] result = (T[]) Array.newInstance(f.elementType(), list.size());
		
		int i = 0;
		for (S s : list) {
			result[i++] = f.map(s);
		}
		return result;
	}
	
	public static <T> List<T> filtMap(ArrayMapFunc<T> f, Object[] list) {
	    LinkedList<T> result = new LinkedList<T>();
	    
	    for (int i = 0; i < list.length; i++) {
	        T o = f.map(list[i]);
	        
	        if (o != null) result.add(o);
	    }
	    
	    return result;
	}
	public static ImageDescriptor findDescriptor(String spath) {
		IPath path = new Path(spath);
		URL url = 
			FileLocator.find(LSLForgePlugin.getDefault().getBundle(), path, null);
		if (url != null) {
			return ImageDescriptor.createFromURL(url);
		} else {
		    return ImageDescriptor.getMissingImageDescriptor();
		}

	}
	
	public static String quote(String s) {
		return new StringBuilder("\"").append(s).append('"').toString(); //$NON-NLS-1$
	}

    public static int elementIndex(Object o, Object[] os) {
        if (o == null || os == null) return -1;
        
        for (int i = 0; i < os.length; i++) {
            if (o.equals(os[i])) return i;
        }
        return -1;
    }

    public static <T> Set<T> mapToSet(ArrayMapFunc<T> arrayMapFunc, List<T> globs) {
        HashSet<T> set = new HashSet<T>();
        for (Iterator<T> i = globs.iterator(); i.hasNext(); ) {
            set.add(arrayMapFunc.map(i.next()));
        }
        
        return set;
    }
    
    public static boolean safeEquals(Object o0, Object o1) {
        return o0 == o1 || (o0 != null && o0.equals(o1));
    }
    
    private final static char[] DIGITS =
        { '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F' };
    public static String URIEncode(String s) {
        StringBuilder buf = new StringBuilder();
        StringBuilder tmp = new StringBuilder();
        char[] cs = s.toCharArray();
        
        for (int i = 0; i < cs.length; i++) {
           if (shouldEncode(cs[i])) {
               tmp.append(cs[i]);
               byte[] b;
               try {
                    b = tmp.toString().getBytes("UTF-8"); //$NON-NLS-1$
                    tmp.setLength(0);
                    for (int j = 0; j <  b.length; j++) {
                        buf.append('%');
                        buf.append(DIGITS[0x0f & (b[j]>>4)]);
                        buf.append(DIGITS[0x0f & b[j]]);
                    }
               } catch (UnsupportedEncodingException e) {
                   throw new RuntimeException(e);
               }
           } else buf.append(cs[i]);
        }
        return buf.toString();
    }

    public static String URIDecode(String s) {
        StringBuilder buf = new StringBuilder();
        
        char[] cs = s.toCharArray();
        
        for (int i = 0; i < cs.length; i++) {
            if (cs[i] == '%') {
                if (i > cs.length - 3) {
                    buf.append('%');
                    break;
                }
                char c1 = cs[++i];
                char c2 = cs[++i];
                
                char c3 = Character.toLowerCase(c1);
                char c4 = Character.toLowerCase(c2);
                
                if ((c3 >= 0 && c3 <= '9' || c3 >= 'a' && c3 <= 'f') &&
                        (c4 >= 0 && c4 <= '9' || c4 >= 'a' && c4 <= 'f')) {
                    char c = (char)(
                         (((c3 <= '9') ? (c3 - '0') : 10 + c3 - 'a') << 4) |
                         (((c4 <= '9') ? (c4 - '0') : 10 + c4 - 'a')));
                    buf.append(c);
                } else {
                    buf.append('%').append(c1).append(c2);
                }
            } else buf.append(cs[i]);
        }
        
        return buf.toString();
    }
    private static boolean shouldEncode(char c) {
        return !((c >= 'a' && c <= 'z') ||
                 (c >= 'A' && c <= 'Z') ||
                 (c >= '0' && c <= '9') ||
                 c == '-' || c == '_' || c == '.' || c == '~');
    }
    
    public static void chmod(File f) throws IOException {
        ProcessBuilder builder = new ProcessBuilder(new String[] {"chmod", "+x", f.getAbsolutePath()});  //$NON-NLS-1$//$NON-NLS-2$
        builder.redirectErrorStream(true);
        Process p = builder.start();
        
        InputStreamReader reader = new InputStreamReader(p.getInputStream());
        int c;
        
        while ((c = reader.read()) >= 0) {
            char cc = (char)c;
            System.out.print(cc);
        }
    }
    
	public static <T> T ctxItem(Ctx<T> c) {
		T o = ((Ctx_Ctx<T>)c).ctxItem;
		return o;
	}

	public static boolean isModule(IResource resource) {
		return "lslm".equals(resource.getProjectRelativePath().getFileExtension()); //$NON-NLS-1$
	}

	public static boolean isScript(IFile file) {
		return isScript(false, file);
	}
	
	public static boolean isScript(boolean strict, IFile file) {
		if("lslp".equals(file.getProjectRelativePath().getFileExtension())) return true; //$NON-NLS-1$
		
		if(!strict) {
			if("lsl".equals(file.getProjectRelativePath().getFileExtension())) { //$NON-NLS-1$
				//This file may be a script, if there's no matching .lslp file
				String name = file.getLocationURI().toString() + "p"; //$NON-NLS-1$
				try {
					URI pFile = new URI(name);
					IPath pPath = new Path(pFile.getPath());
					if(!ResourcesPlugin.getWorkspace().getRoot().exists(pPath)) {
						return true;
					}
				} catch (URISyntaxException e) {
					//Assume the worst
					return false;
				}
			}
		}
		return false;
	}
	
	public static IFile getScriptCompiledName(IFile file) {
		if("lslp".equals(file.getProjectRelativePath().getFileExtension())) { //$NON-NLS-1$
			IPath newpath = file.getFullPath().removeFileExtension().addFileExtension("lsl"); //$NON-NLS-1$
			return file.getWorkspace().getRoot().getFile(newpath);
		}
		
		return null;
	}
}
