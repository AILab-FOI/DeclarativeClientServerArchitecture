import { Component, model, output, signal } from '@angular/core';
import { defaultKorisnik, Korisnik } from '../../core';
import { InputComponent } from '../input/input.component';
import { send_multipart, upload_file } from '../../../assets/pkg/client';

@Component({
  selector: 'app-user-edit',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './user-edit.component.html',
  styleUrl: './user-edit.component.scss',
})
export class UserEditComponent {
  data = model<Korisnik>(defaultKorisnik());
  query = output<Korisnik>();
  imgSource = signal<any>(undefined);

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  async fileUpload(event: Event) {
    const file = (event.target as HTMLInputElement).files?.[0];

    if (file) {
      const reader = new FileReader();
      reader.onload = () => {
        this.imgSource.set(reader.result);
      };
      reader.readAsDataURL(file);
      let asd = await upload_file('korisnik', `${this.data().id}`, file);
    }
  }
}
